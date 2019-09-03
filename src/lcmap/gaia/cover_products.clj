(ns lcmap.gaia.cover-products
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.string        :as string]
            [clojure.tools.logging :as log]
            [java-time             :as jt]
            [lcmap.gaia.config     :refer [config]]
            [lcmap.gaia.file       :as file]
            [lcmap.gaia.gdal       :as gdal]
            [lcmap.gaia.product-specs :as product-specs]
            [lcmap.gaia.storage    :as storage]
            [lcmap.gaia.util       :as util]))

(defn product-exception-handler
  [exception product_name pixel]
  (let [msg (format "problem calculating %s with pixel %s: %s" product_name pixel (.getMessage exception))]
    (log/error msg)
    (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause exception)))))

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
  (let [matching_predictions (filter (fn [i] (= class_val (get-class (get i "prob")))) sorted_predictions)]
      (get (first matching_predictions) "pday")))

(defn mean-probabilities
  "Returns a 1-d collection of mean probabilities given a collection of probabilities "
  [predictions]
  (let [probabilities (map #(get % "prob") predictions)
        indexes (range 0 (count (first probabilities)))
        mean_fn (fn [i] (util/mean (map #(nth % i) probabilities)))]
    (map mean_fn indexes)))

(defn class-details
  [predictions query_date rank burn_ratio]
  (let [first_class (get-class (get (first predictions) "prob"))
        last_class  (get-class (get (last predictions) "prob"))
        grass (:grass (:lc_map config))
        tree  (:tree  (:lc_map config))
        first_forest_date (util/to-ordinal (first-date-of-class predictions tree))   
        first_grass_date  (util/to-ordinal (first-date-of-class predictions grass))    
        growth (and (> burn_ratio 0.05) (= grass first_class) (= tree last_class))
        decline (and (< burn_ratio -0.05) (= tree first_class) (= grass last_class))
        probabilities (mean-probabilities predictions)
        class (get-class probabilities rank)]
    (hash-map
     :first_class first_class
     :last_class  last_class
     :first_forest_date first_forest_date
     :first_grass_date first_grass_date
     :mean_probabilities probabilities
     :growth growth
     :decline decline
     :class class)))

(defn classify
  "Return the classification value for a single segment given a query_day and rank"
  [predictions query_date rank burn_ratio]
  (let [grass (:grass (:lc_map config))
        tree  (:tree  (:lc_map config))
        details (class-details predictions query_date rank burn_ratio)]

    (cond
     (:growth details)
     (if (>= query_date (:first_forest_date details))
       (nth [tree grass] rank)
       (nth [grass tree] rank))

     (:decline details)
     (if (>= query_date (:first_grass_date details))
       (nth [grass tree] rank)
       (nth [tree grass] rank))

     :else
     (:class details))))

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
        ordinal_sday #(util/to-ordinal (get % "sday"))
        probability_reducer (fn [coll p] (if (= (ordinal_sday p) sday) (conj coll p) coll)) ; if prediction sday == segment sday, keep it
        segment_probabilities (reduce probability_reducer [] probabilities)
        sorted_probabilities  (util/sort-by-key segment_probabilities "pday")
        primary_classification   (classify sorted_probabilities query_day 0 burn_ratio)
        secondary_classification (classify sorted_probabilities query_day 1 burn_ratio)
        class_details (class-details sorted_probabilities query_day 0 burn_ratio)]
    (hash-map :intersects      intersects
              :precedes_sday   precedes_sday
              :follows_eday    follows_eday
              :follows_bday    follows_bday
              :btw_eday_bday   between_eday_bday
              :sday            sday
              :eday            eday
              :bday            bday
              :growth          (:growth class_details)
              :decline         (:decline class_details)
              :chprob          chprob
              :probabilities   sorted_probabilities
              :primary_class   primary_classification
              :secondary_class secondary_classification)))

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
         ; query date precedes first segment start date
         (< query_date first_start_day)
         (class_key (first segments)) ; return value of the first segment

         ; query date follows last segment end date
         (> query_date last_end_day)
         (class_key (last segments)) ; return value of the last segment
         
         ; query date falls between a segments start date and end date
         (not (nil? intersected_segment))
         (class_key intersected_segment) ; return the class value for the intercepted model

         ; query date falls between segments of same landcover classification and fill_samelc config is true
         (and (:fill_samelc conf) (= (class_key (first between_eday_sday)) (class_key (last between_eday_sday))))
         (class_key (last between_eday_sday)) ; return the value from the last model from the pair of models the query date fell between

         ; query date falls between one segments break date and the following segments start date and fill_difflc config is true
         (and (:fill_difflc conf) (not (map? between_bday_sday)))
         (class_key (last between_bday_sday)) ; return the value from the last model from the pair of models the query date fell between

         ; query date falls between a segments end date and break date and fill_difflc config is true
         (and (:fill_difflc conf) (not (nil? eday_bday_model)))
         (class_key eday_bday_model) ; return the value from the model where the query date intersected the end date and break date

         :else ; we need to throw an exception
         (throw (ex-info (format "Landcover value calculation problem, unclassifiable pixel %s" (:pixelxy characterized_pixel)) 
                         {:type "data-generation-error"}))))

     (catch Exception e
       (product-exception-handler e "landcover" characterized_pixel))))
  ([characterized_pixel rank] ; enable passing in the configuration
   (landcover characterized_pixel rank config)))

(defn change
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
           [px py]             (:pixelxy characterized_pixel)
           segments            (:segments characterized_pixel) ;characterized and sorted
           first_start_day     (:sday (first segments))
           last_end_day        (:eday (last segments))
           intersected_segment (first (filter :intersects segments))
           eday_bday_model     (first (filter :btw_eday_bday segments))
           between_eday_sday   (reduce falls-between-eday-sday segments)
           between_bday_sday   (reduce falls-between-bday-sday segments)]

       (cond
        ; first segment wasn't classifiable, and query date falls between segments
        (and (= 2 (count between_eday_sday)) (empty? (:probabilities (first between_eday_sday))))
        (:lcc_back (:lc_defaults conf))

        ; last segment wasn't classifiable, and query date falls between segments
        (and (= 2 (count between_eday_sday)) (empty? (:probabilities (last between_eday_sday))))
        (:lcc_afterbr (:lc_defaults conf))

        ; query date precedes first segment start date
        (< query_date first_start_day)
        (:lcc_back (:lc_defaults conf)) ; return lcc_back value from lc_defaults config

        ; query date follows last segment end date and either change prob == 1 or last segment has no predictions
        (and (> query_date last_end_day) 
             (or (= 1 (int (:chprob (last segments))))
                 (empty? (:probabilities (last segments)))))
        (:lcc_afterbr (:lc_defaults conf)) ; return the lcc_afterbr value from the lc_defaults config

        ; query date follows last segment end date
        (> query_date last_end_day)
        (:lcc_forwards (:lc_defaults conf)) ; return the lcc_forwards value from the lc_defaults config

        ; query date falls between a segments start date and end date and growth is true
        (and (not (nil? intersected_segment)) (:growth intersected_segment))
        (:lcc_growth (:lc_defaults conf)) ; return lcc_growth value from lc_defaults config

        ; query date falls between a segments start date and end date and decline is true
        (and (not (nil? intersected_segment)) (:decline intersected_segment))
        (:lcc_decline (:lc_defaults conf)) ; return lcc_decline value from lc_defaults config

        ; query date falls between a segments start date and end date
        (and (not (nil? intersected_segment)) (not (empty? (:probabilities intersected_segment))))
        (-> (:probabilities intersected_segment) mean-probabilities sort reverse (nth rank) util/scale-value)

        ; query date falls between segments of same landcover classification
        (= (:primary_class (first between_eday_sday)) (:primary_class (last between_eday_sday)))
        (:lcc_samelc (:lc_defaults conf)) ; return lcc_samelc from lc_defaults config

        ; query date falls between segments with different landcover classifications
        (= 2 (count between_eday_sday))
        (:lcc_difflc (:lc_defaults conf)) ; return lcc_difflc from lc_defaults config

        :else ; we need to throw and exception
        (throw (ex-info (format "Confidence value calculation problem, pixel %s" (:pixelxy characterized_pixel)) 
                         {:type "data-generation-error"}))))
     (catch Exception e
       (product-exception-handler e "confidence" characterized_pixel))))
  ([characterized_pixel rank]
   (confidence characterized_pixel rank config)))

(defn characterize-inputs
  "Return a hash-map characterizing details of the segment"
  [pixelxy inputs query_day]
  (let [segments_valid (product-specs/segments-valid? (:segments inputs))
        predictions_valid (product-specs/predictions-valid? (:predictions inputs))
        response    #(hash-map :pixelxy pixelxy :segments % :date query_day)]
    (if (and segments_valid predictions_valid)
      (response (map #(characterize-segment % query_day (:predictions inputs)) (:segments inputs)))
      (response []))))

(defn products 
  [characterized_pixel]
  (let [date    (:date characterized_pixel)
        [px py] (:pixelxy characterized_pixel)
        none    (:none (:lc_map config))
        nomodel (:lcc_nomodel (:lc_defaults config))
        good_data            (not (empty? (:segments characterized_pixel)))
        primary_landcover    (if good_data (landcover  characterized_pixel 0) none) 
        secondary_landcover  (if good_data (landcover  characterized_pixel 1) none)
        primary_confidence   (if good_data (confidence characterized_pixel 0) nomodel) 
        secondary_confidence (if good_data (confidence characterized_pixel 1) nomodel)
        annual_change        (if good_data (change     characterized_pixel)   none)]

    (hash-map :px px :py py :date date
              :values {:primary-landcover primary_landcover 
                       :secondary-landcover secondary_landcover
                       :primary-confidence primary_confidence
                       :secondary-confidence secondary_confidence
                       :annual-change annual_change})))

(defn generate
  [{dates :dates cx :cx cy :cy tile :tile :as all}]
  (try
    (let [segments             (util/with-retry (storage/segments-sorted cx cy "sday")) 
          predictions          (util/with-retry (storage/predictions cx cy)) 
          grouped_segments     (util/pixel-groups segments)
          grouped_predictions  (util/pixel-groups predictions)
          pixel_coords         (keys grouped_segments)
          pixel_hash-map       #(hash-map % {:segments (get grouped_segments %) :predictions (get grouped_predictions %)})
          pixel_inputs         (into {} (map pixel_hash-map pixel_coords))]

      (doseq [date dates]
        (let [ordinal_date     (util/to-ordinal date)
              pixel_dates      (combo/cartesian-product [ordinal_date] (keys pixel_inputs)) ; ([ordinal-date [px py]], ...)]
              comp_fn          (comp products #(characterize-inputs (last %) (get pixel_inputs (last %)) (first %)))
              pixel_products   (pmap comp_fn pixel_dates)
              path             (storage/ppath "cover" cx cy tile date)
              time_message     (format "Cover product calculation for tile:%s cx:%s cy:%s date:%s " tile cx cy date)
              flattened_values (util/log-time (util/flatten-values pixel_products) time_message)]
          (log/infof "storing : %s" (:name path))
          (util/with-retry (storage/put_json path flattened_values))))

      {:products "cover" :cx cx :cy cy :dates dates})
    (catch Exception e
      (let [msg (format "problem generating cover products for %s: %s" all (.getMessage e))]
        (log/error msg)
        (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause e)))))))
