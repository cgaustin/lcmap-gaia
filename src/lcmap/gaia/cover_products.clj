(ns lcmap.gaia.cover-products
  (:gen-class)
  (:require [again.core :as again] 
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.stacktrace    :as stacktrace]
            [clojure.string        :as string]
            [clojure.tools.logging :as log]
            [clojure.walk          :refer [keywordize-keys]]
            [java-time             :as jt]
            [lcmap.gaia.config     :refer [config]]
            [lcmap.gaia.file       :as file]
            [lcmap.gaia.gdal       :as gdal]
            [lcmap.gaia.nemo       :as nemo]
            [lcmap.gaia.product-specs :as product-specs]
            [lcmap.gaia.storage    :as storage]
            [lcmap.gaia.util       :as util]))

(def product_details
  (hash-map "primary-landcover"              {:abbr "LCPRI"   :type gdal/int8}              
            "secondary-landcover"            {:abbr "LCSEC"   :type gdal/int8}            
            "primary-landcover-confidence"   {:abbr "LCPCONF" :type gdal/int8}  
            "secondary-landcover-confidence" {:abbr "LCSCONF" :type gdal/int8} 
            "annual-change"                  {:abbr "LCACHG"  :type gdal/int8}))

(defn convert_prediction_dates 
  [inpred]
  (let [to-ord #(util/to-ordinal (% inpred))]
    (merge inpred {:sday (to-ord :sday) :eday (to-ord :eday) :pday (to-ord :pday)})))

(defn get-prefix
  ([grid date tile type product]
   (let [hhh (subs tile 0 3)
         vvv (subs tile 3 6)
         year (first (string/split date #"-"))
         elements [type year grid hhh vvv product]]
     (string/join "/" elements)))
  ([grid date tile type product cx cy]
   (let [prfx (get-prefix grid date tile type product)
         elements [prfx cx cy]]
     (string/join "/" elements))))

(defn map-path
  [tileid product date]
  (let [grid      (:region config)
        repr_date (string/replace date "-" "")
        ccd_ver   (:ccd_ver config)
        product_abbr (:abbr (get product_details product)) 
        elements ["LCMAP" grid tileid repr_date ccd_ver product_abbr]
        name (str (string/join "-" elements) ".tif")
        prefix (get-prefix grid date tileid "raster" product)
        url (storage/get_url storage/bucketname (str prefix "/" name))]
    {:name name :prefix prefix :url url}))

(defn ppath
  ([product x y tile date suffix]
   (let [grid (:region config)
         fx   (util/float-string x)
         fy   (util/float-string y)
         name (->> [product fx fy date] (string/join "-") (#(str % suffix)))
         prefix (get-prefix grid date tile "json" product fx fy)]
     {:name name :prefix prefix}))
  ([product x y tile date]
   (ppath product x y tile date ".json")))


(defn product-exception-handler
  [exception product_name]
  (let [type    (keyword (str product_name "-exception"))
        message (str "Error calculating " product_name)]
    (log/errorf "%s: %s  stacktrace: %s" 
                message product_name (stacktrace/print-stack-trace exception))
    (throw (ex-info message {:type "data-generation-error" 
                             :message type 
                             :exception exception}))))


(defn falls-between-eday-sday
  "Convenience function for returning pair of maps with true values for :follows_eday and :precedes_sday keys.
   Used with Reduce to identify maps in a list of sorted maps"
  [map_a map_b]
  (util/matching-keys map_a map_b :follows_eday :precedes_sday true))

(defn falls-between-bday-sday
  "Convenience function for returning pair of maps with true values for :follows_bday and :precedes_sday keys.
   Used with Reduce to identify maps in a list of sorted maps"
  [map_a map_b]
  (util/matching-keys map_a map_b :follows_bday :precedes_sday true))

(defn normalized-burn-ratio
  "Return the Normalized Burn Ratio for a segment"
  [model sday eday]
  (let [niint  (get model "niint")
        s1int  (get model "s1int")
        nicoef (first (get model "nicoef"))
        s1coef (first (get model "s1coef"))
        nir_start  (+ niint (* sday nicoef))
        nir_end    (+ niint (* eday nicoef))
        swir_start (+ s1int (* sday s1coef))
        swir_end   (+ s1int (* eday s1coef))
        nbr_start  (float (/ (- nir_start swir_start) (+ nir_start swir_start)))
        nbr_end    (float (/ (- nir_end swir_end) (+ nir_end swir_end)))] 
    (- nbr_end nbr_start)))

(defn get-class 
 "Returns the class value given a collection of probabilities"
  ([probs rank]
   (try
     (let [sorted (reverse (sort probs)) 
           position (.indexOf probs (nth sorted rank))]
       (nth (:lc_list config) position))
     (catch IndexOutOfBoundsException e ; the probs collection is empty
       (:none (:lc_map config)))))      ; return configured value for None
  ([probs]
   (get-class probs 0)))

(defn first-date-of-class
  "Returns the 'date' value from a collection of predictions for the first occurence of a given classification"
  [sorted_predictions class_val]
  (let [matching_predictions (filter (fn [i] (= class_val (get-class (:prob i)))) sorted_predictions)]
      (:pday (first matching_predictions))))

(defn mean-probabilities
  "Returns a 1-d collection of mean probabilities given a collection of probabilities "
  [predictions]
  (let [probabilities (map #(get % "prob") predictions)
        indexes (range 0 (count (first probabilities)))
        mean_fn (fn [i] (util/mean (map #(nth % i) probabilities)))]
    (map mean_fn indexes)))

(defn classify
  "Return the classification value for a single segment given a query_day and rank"
  [model query_date rank burn_ratio]
  (let [predictions (:probabilities model) ; sorted in characterize-segment
        first_class ((comp get-class #(get % "prob") first) predictions) ;(-> predictions (first) (:prob) (get-class))
        last_class  ((comp get-class #(get % "prob") last) predictions)  ;(-> predictions (last)  (:prob) (get-class))
        grass (:grass (:lc_map config))
        tree  (:tree  (:lc_map config))
        first_forest_date (first-date-of-class predictions tree)  ; (-> predictions (first-date-of-class tree))   
        first_grass_date  (first-date-of-class predictions grass) ; (-> predictions (first-date-of-class grass))    
        probabilities (mean-probabilities predictions)]

    (cond
     ; burn_ratio > 0.05 and first_class is 'grass' and last is 'forest'
     (= true (> burn_ratio 0.05) (= grass first_class) (= tree last_class))
     (if (>= query_date first_forest_date)
       (nth [tree grass] rank)
       (nth [grass tree] rank))

     ; burn_ratio < -0.05 and last class is grass and first class is forest
     (= true (< burn_ratio -0.05) (= tree first_class) (= grass last_class))
     (if (>= query_date first_grass_date)
       (nth [grass tree] rank)
       (nth [tree grass] rank))

     :else ; calculate the mean across all probabilities for the segment, classify based on highest probability
     (get-class probabilities rank))))

(defn characterize-segment
  "Return a hash-map characterizing details of the segment"
  [segment query_day probabilities]
  (let [sday ((comp util/to-ordinal #(get % "sday")) segment)
        eday ((comp util/to-ordinal #(get % "eday")) segment)
        bday ((comp util/to-ordinal #(get % "bday")) segment)
        chprob (get segment "chprob")
        burn_ratio (normalized-burn-ratio segment sday eday)
        intersects        (<= sday query_day eday)
        precedes_sday     (< query_day sday)
        follows_eday      (> query_day eday)
        follows_bday      (>= query_day bday)
        between_eday_bday (<= eday query_day bday)
        growth  (> burn_ratio 0.05)
        decline (< burn_ratio -0.05)
        ordinal_sday #(util/to-ordinal (get % "sday"))
        probability_reducer (fn [coll p] (if (= (ordinal_sday p) sday) (conj coll p) coll)) ; if prediction sday == segment sday, keep it
        segment_probabilities (reduce probability_reducer [] probabilities)
        sorted_probabilities  (util/sort-by-key segment_probabilities "pday")
        primary_classification   (classify (merge segment {:probabilities sorted_probabilities}) query_day 0 burn_ratio)
        secondary_classification (classify (merge segment {:probabilities sorted_probabilities}) query_day 1 burn_ratio)]
    (hash-map :intersects      intersects
              :precedes_sday   precedes_sday
              :follows_eday    follows_eday
              :follows_bday    follows_bday
              :btw_eday_bday   between_eday_bday
              :sday            sday
              :eday            eday
              :bday            bday
              :growth          growth
              :decline         decline
              :chprob          chprob
              :probabilities   sorted_probabilities
              :primary_class   primary_classification
              :secondary_class secondary_classification)))

; {:date 730666, :segments ({:bday 736594, :intersects true, :follows_bday false, :btw_eday_bday false, :sday 724514, :decline false, :eday 736594, :follows_eday false, :primary_class 2, :precedes_sday false, :secondary_class 1, :growth true}), :pixelxy [1634775 2066595]}
(defn landcover
  "Return the landcover value given the segments, probabilities, query_day and rank for a location"
  ([characterized_pixel rank conf]
   (try
     (let [segments            (:segments characterized_pixel) ;characterized and sorted
           query_date          (:date characterized_pixel)
           first_start_day     (:sday (first segments))
           last_end_day        (:eday (last segments))
           intersected_segment (first (filter :intersects segments))
           eday_bday_model     (first (filter :btw_eday_bday segments))
           between_eday_sday   (reduce falls-between-eday-sday segments)
           between_bday_sday   (reduce falls-between-bday-sday segments)
           class_key           (if (= 0 rank) :primary_class :secondary_class)]

       (cond
         ; query date precedes first segment start date and fill_begin config is true
         (= true (< query_date first_start_day) (:fill_begin conf))
         (class_key (first segments)) ; return value of the first segment

         ; query date precedes first segment start date
         (= true (< query_date first_start_day))
         (:lc_insuff (:lc_defaults conf)) ; return lc_insuff value from lc_defaults config

         ; query date follows last segment end date and fill_end config is true
         (= true (> query_date last_end_day) (:fill_end conf))
         (class_key (last segments)) ; return value of the last segment
         
         ; query date follows last segment end date
         (= true (> query_date last_end_day))
         (:lc_insuff (:lc_defaults conf)) ; return the lc_insuff value from the lc_defaults config

         ; query date falls between a segments start date and end date
         (not (nil? intersected_segment))
         (class_key intersected_segment) ; return the class value for the intercepted model

         ; query date falls between segments of same landcover classification and fill_samelc config is true
         (= true (:fill_samelc conf) (= (class_key (first between_eday_sday)) (class_key (last between_eday_sday))))
         (class_key (last between_eday_sday)) ; return the value from the last model from the pair of models the query date fell between

         ; query date falls between one segments break date and the following segments start date and fill_difflc config is true
         (= true (:fill_difflc conf) (not (map? between_bday_sday)))
         (class_key (last between_bday_sday )) ; return the value from the last model from the pair of models the query date fell between

         ; query date falls between a segments end date and break date and fill_difflc config is true
         (= true (:fill_difflc conf) (not (nil? eday_bday_model)))
         (class_key eday_bday_model) ; return the value from the model where the query date intersected the end date and break date

         :else ; finally as a last resort return the lc_inbtw value from the configuration
         (:lc_inbtw conf)))

     (catch Exception e
       (product-exception-handler e "landcover"))))
  ([characterized_pixel rank] ; enable passing in the configuration
   (landcover characterized_pixel rank config)))

(defn annual-change
  "Return the change in landcover from the provided year, to the previous year"
  [characterized_pixel]
  (let [query_day          (:date characterized_pixel)
        previous_query_day (util/subtract_year query_day)
        previous_pixel     (merge characterized_pixel {:date previous_query_day})
        current_landcover  (landcover characterized_pixel 0)
        previous_landcover (landcover previous_pixel 0)]

    (if (= current_landcover previous_landcover)
      current_landcover
      (util/concat_ints previous_landcover current_landcover))))

(defn confidence
  "Return the landcover confidence value given the segments, probabilities, query_day and rank for a location"
  ([characterized_pixel rank conf]
   (try
     (let [query_date          (:date characterized_pixel)
           segments            (:segments characterized_pixel) ;characterized and sorted
           first_start_day     (:sday (first segments))
           last_end_day        (:eday (last segments))
           intersected_segment (first (filter :intersects segments))
           eday_bday_model     (first (filter :btw_eday_bday segments))
           between_eday_sday   (reduce falls-between-eday-sday segments)
           between_bday_sday   (reduce falls-between-bday-sday segments)]

       (cond
        ; query date preceds first segment start date
        (= true (< query_date first_start_day))
        (:lcc_back (:lc_defaults conf)) ; return lcc_back value from lc_defaults config

        ; query date follows last segment end date and change prob == 1
        (= true (> query_date last_end_day) (= 1 (int (:chprob (last segments)))))
        (:lcc_afterbr (:lc_defaults conf)) ; return the lcc_afterbr value from the lc_defaults config

        ; query date follows last segment end date
        (= true (> query_date last_end_day))
        (:lcc_forwards (:lc_defaults conf)) ; return the lcc_forwards value from the lc_defaults config

        ; query date falls between a segments start date and end date and growth is true
        (= true (not (nil? intersected_segment)) (:growth intersected_segment))
        (:lcc_growth (:lc_defaults conf)) ; return lcc_growth value from lc_defaults config

        ; query date falls between a segments start date and end date and decline is true
        (= true (not (nil? intersected_segment)) (:decline intersected_segment))
        (:lcc_decline (:lc_defaults conf)) ; return lcc_decline value from lc_defaults config

        ; query date falls between a segments start date and end date
        (not (nil? intersected_segment))
        (util/scale-value (nth (get (last (:probabilities intersected_segment)) "prob") rank))

        ; query date falls between segments of same landcover classification
        (= true (= (:primary_class (first between_eday_sday)) (:primary_class (last between_eday_sday))))
        (:lcc_samelc (:lc_defaults conf)) ; return lcc_samelc from lc_defaults config

        ; query date falls between segments with different landcover classifications
        (= 2 (count between_eday_sday))
        (:lcc_difflc (:lc_defaults conf)) ; return lcc_difflc from lc_defaults config

        :else ; mapify returns ValueError
        (:none (:lc_map conf))))

     (catch Exception e
       (product-exception-handler e "confidence"))))
  ([characterized_pixel rank]
   (confidence characterized_pixel rank config)))

(defn characterize-inputs
  "Return a hash-map characterizing details of the segment"
  [pixelxy inputs query_day]
  (let [segments    (:segments inputs)
        predictions (:predictions inputs)
        characterized (map #(characterize-segment % query_day predictions) segments)]
    (hash-map :pixelxy pixelxy :segments characterized :date query_day)))

(defn products 
  [characterized_pixel]
  (let [date (:date characterized_pixel)
        [px py] (:pixelxy characterized_pixel)
        primary_landcover    (landcover  characterized_pixel 0)
        secondary_landcover  (landcover  characterized_pixel 1)
        primary_confidence   (confidence characterized_pixel 0)
        secondary_confidence (confidence characterized_pixel 1)
        annual_change        (annual-change characterized_pixel)]

    (hash-map :px px :py py :date date
              :values {:primary-landcover primary_landcover 
                       :secondary-landcover secondary_landcover
                       :primary-confidence primary_confidence
                       :secondary-confidence secondary_confidence
                       :annual-change annual_change})))

(defn retry-handler [i cause]
  (let [exception (::again/exception i)
        data (ex-data exception)]
    (when exception
      (do
        (if (= cause (:cause data))
          ::again/fail
          (log/infof "retrying chip: %s" data))))))

(defn generate
  [{dates :dates cx :cx cy :cy tile :tile :as all}]
  (try
    (let [segments             (nemo/segments cx cy)
          predictions          (nemo/predictions cx cy)
          grouped_segments     (util/pixel-groups segments)
          grouped_predictions  (util/pixel-groups predictions)
          get_sorted           #(util/sort-by-key (get grouped_segments %) "sday")
          pixel_inputs         (into {} (map #(hash-map % {:segments (get_sorted %) :predictions (get grouped_predictions %)} ) (keys grouped_segments))) 
          ordinal_dates        (map util/to-ordinal dates)
          pixel_dates          (combo/cartesian-product ordinal_dates (keys pixel_inputs)) ; ([ordinal-date [px py]], ...)
          characterized_pixels (map #(characterize-inputs (last %) (get pixel_inputs (last %)) (first %)) pixel_dates)
          pixel_products       (map products characterized_pixels)
          grouped_products     (group-by :date pixel_products)]

      (doseq [[date values] grouped_products]
        (let [path (ppath "cover" cx cy tile (util/to-yyyy-mm-dd date))
              flattened_values (util/flatten-values values)
              destination (string/join "/" [(:prefix path) (:name path)])]
          (log/infof "storing : %s" (:name path))
          (again/with-retries (:retry_strategy config)
            (storage/put_json destination flattened_values)))))
    (catch Exception e
      (log/errorf "Exception in products/generation - args: %s  message: %s  data: %s  stacktrace: %s"
                  all (.getMessage e) (ex-data e) (stacktrace/print-stack-trace e))
      (throw (ex-info "Exception in products/generate" {:type "data-generation-error"
                                                        :message (.getMessage e)
                                                        :args all})))))
