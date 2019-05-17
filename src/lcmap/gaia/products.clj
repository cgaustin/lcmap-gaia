(ns lcmap.gaia.products
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
            "annual-change"                  {:abbr "LCACHG"  :type gdal/int8} 
            "time-of-change"                 {:abbr "SCTIME"  :type gdal/int16}                
            "magnitude-of-change"            {:abbr "SCMAG"   :type gdal/float32}            
            "time-since-change"              {:abbr "SCLAST"  :type gdal/int16}              
            "curve-fit"                      {:abbr "SCMQA"   :type gdal/int8}                     
            "length-of-segment"              {:abbr "SCSTAB"  :type gdal/int16}))

(defn is-landcover
  [name]
  (let [abbr (:abbr (get product_details name))]
    (try
      (some? (re-matches #"LC(.*)" abbr))
      (catch NullPointerException e
        false))))

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
   (let [grid      (:region config)
         name (->> [product x y date] (string/join "-") (#(str % suffix)))
         prefix (get-prefix grid date tile "json" product x y)]
     {:name name :prefix prefix}))
  ([product x y tile date]
   (ppath product x y tile date ".json")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;    CHANGE PRODUCTS    ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn product-exception-handler
  [exception product_name]
  (let [type_name (keyword (str product_name "-exception"))
        message (str "Error calculating " product_name)]
    (log/errorf (str message exception))
    (throw (ex-info message {:type type_name :cause :product-failure :exception exception}))))

(defn time-of-change
  "Return numeric day of year in which a break occurs"
  ([model query-day]
   (try
     (let [change-prob (:chprob model)
           break-day   (-> model (:bday) (util/to-javatime)) 
           query-year  (-> query-day (util/ordinal-to-javatime) (util/javatime-year))
           break-year  (-> break-day (util/javatime-year))]
       (if (and (= query-year break-year) (= 1.0 change-prob))
         (util/javatime-day-of-year break-day)
         0))
     (catch Exception e
       (product-exception-handler e "time-of-change"))))
  ([pixel_map pixel_models query-day]
   (let [segments (filter product-specs/segment_valid? (:segments pixel_models))
         values   (map #(time-of-change % query-day) segments)
         response #(hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val %)]
     (if (empty? segments)
       (-> 0 (response))
       (-> (last (sort values)) (response))))))

(defn time-since-change
  "Return cumulative distance to previous break"
  ([model query-day]
   (try
     (let [change-prob (= 1.0 (:chprob model)) 
           break-day   (-> model (:bday) (util/to-ordinal))
           day-diff    (- query-day break-day)]
       (if (and change-prob (>= day-diff 0)) 
                         day-diff 
                         nil))
     (catch Exception e
       (product-exception-handler e "time-since-change"))))
  ([pixel_map pixel_models query-day]
   (let [segments  (filter product-specs/segment_valid? (:segments pixel_models))
         values    (map #(time-since-change % query-day) segments)
         valid     (filter number? values)
         response #(hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val %)]
     (if (or (empty? segments) (empty? valid))
       (-> 0 (response))
       (-> (first (sort valid)) (response))))))

(defn magnitude-of-change
  "Return severity of spectral shift"
  ([model query-day]
   (try
     (let [change-prob (= 1.0 (:chprob model)) 
           query-year  (-> query-day (util/ordinal-to-javatime) (util/javatime-year))
           break-year  (-> (:bday model) (util/to-javatime) (util/javatime-year))
           magnitudes  [(:grmag model) (:remag model) (:nimag model) (:s1mag model) (:s2mag model)]
           euc-norm    (math/sqrt (reduce + (map #(math/expt % 2) magnitudes)))]
       (if (and (= query-year break-year) change-prob)
         euc-norm
         0))
     (catch Exception e
       (product-exception-handler e "magnitude-of-change"))))
  ([pixel_map pixel_models query-day]
   (let [segments (filter product-specs/segment_valid? (:segments pixel_models))
         values   (map #(magnitude-of-change % query-day) segments)
         response  #(hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val %)]
     (if (empty? segments)
       (-> 0 (response))
       (-> (last (sort values)) (response))))))

(defn length-of-segment
  "Return length of change segment in days"
  ([model query-day]
   (try
     (let [fill (- query-day (util/to-ordinal (:stability_begin config)))
           start-day (-> model (:sday) (util/to-ordinal))
           end-day   (-> model (:eday) (util/to-ordinal))
           diff      (if (> query-day end-day) (- query-day end-day) (- query-day start-day))]
       (if (and (<= 0 diff) (< diff fill)) 
         diff 
         fill))
     (catch Exception e
       (product-exception-handler e "length-of-segment"))))
  ([pixel_map pixel_models query-day]
   (let [fill     (- query-day (util/to-ordinal (:stability_begin config)))
         segments (filter product-specs/segment_valid? (:segments pixel_models))
         values   (map #(length-of-segment % query-day) segments)
         response #(hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val %)]
     (if (empty? segments)
       (-> fill (response))
       (-> (first (sort values)) (response))))))

(defn curve-fit
  "Return Curve QA for point in time"
  ([model query-day]
   (try
     (let [curve-qa  (:curqa model)
           start-day (-> model (:sday) (util/to-ordinal))
           end-day   (-> model (:eday) (util/to-ordinal))]
       (if (<= start-day query-day end-day)
         curve-qa
         0))
     (catch Exception e
       (product-exception-handler e "curve-fit"))))
  ([pixel_map pixel_models query-day]
   (let [segments (filter product-specs/segment_valid? (:segments pixel_models))
         values   (map #(curve-fit % query-day) segments)
         response #(hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val %)]
     (if (empty? segments)
       (-> 0 (response))
       (-> (last (sort values)) (response))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;    CLASSIFICATION PRODUCTS    ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  [model]
  (let [sday   (-> model (:sday) (util/to-ordinal)) 
        eday   (-> model (:eday) (util/to-ordinal)) 
        niint  (:niint model)
        s1int  (:s1int model)
        nicoef (-> model (:nicoef) (first))
        s1coef (-> model (:s1coef) (first))
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
   (let [sorted (reverse (sort probs)) 
         position (.indexOf probs (nth sorted rank))]
     (nth (:lc_list config) position)))
  ([probs]
   (get-class probs 0)))

(defn first-date-of-class
  "Returns the 'date' value from a collection of predictions for the first occurence of a given classification"
  [sorted_predictions class_val]
  (let [matching_predictions (filter (fn [i] (= class_val (get-class (:prob i)))) sorted_predictions)]
      (:date (first matching_predictions))))

(defn mean-probabilities
  "Returns a 1-d collection of mean probabilities given a collection of probabilities "
  [predictions]
  (let [probabilities (map :prob predictions)
        indexes (range 0 (count (first probabilities)))
        mean_fn (fn [i] (util/mean (map #(nth % i) probabilities)))]
    (map mean_fn indexes)))

(defn classify
  "Return the classification value for a single segment given a query_day and rank"
  [model query_date rank burn_ratio]
  (let [predictions (:probabilities model) ; sorted in characterize-segment
        first_class (-> predictions (first) (:prob) (get-class))
        last_class  (-> predictions (last)  (:prob) (get-class))
        grass (:grass (:lc_map config))
        tree  (:tree  (:lc_map config))
        first_forest_date (-> predictions (first-date-of-class tree)  (util/to-ordinal))   
        first_grass_date  (-> predictions (first-date-of-class grass) (util/to-ordinal))    
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
  [segment query_day probabilities rank]
  (let [sday (-> segment (:sday) (util/to-ordinal)) 
        eday (-> segment (:eday) (util/to-ordinal))
        bday (-> segment (:bday) (util/to-ordinal))
        burn_ratio (normalized-burn-ratio segment)
        intersects        (<= sday query_day eday)
        precedes_sday     (< query_day sday)
        follows_eday      (> query_day eday)
        follows_bday      (>= query_day bday)
        between_eday_bday (<= eday query_day bday)
        growth  (> burn_ratio 0.05)
        decline (< burn_ratio -0.05)
        segment_probabilities (filter (fn [i] (= (-> i (:sday) (util/to-ordinal)) sday)) probabilities)
        sorted_probabilities  (util/sort-by-key segment_probabilities :date)
        classification (classify (merge segment {:probabilities sorted_probabilities}) query_day rank burn_ratio)]
    (hash-map :intersects     intersects
              :precedes_sday  precedes_sday
              :follows_eday   follows_eday
              :follows_bday   follows_bday
              :btw_eday_bday  between_eday_bday
              :sday           sday
              :eday           eday
              :bday           bday
              :growth         growth
              :decline        decline
              :probabilities  sorted_probabilities
              :classification classification)))

(defn landcover
  "Return the landcover value given the segments, probabilities, query_day and rank for a location"
  ([pixel_models query_date rank conf]
   (try
     (let [sorted_segments (util/sort-by-key (:segments pixel_models) :sday)
           probabilities   (:predictions pixel_models)
           characterized_segments (map #(characterize-segment % query_date probabilities rank) sorted_segments)
           first_start_day   (-> (first sorted_segments) (:sday) (util/to-ordinal))
           last_end_day      (-> (last sorted_segments)  (:eday) (util/to-ordinal))
           intersected_segment (first (filter :intersects characterized_segments))
           eday_bday_model   (first (filter :btw_eday_bday characterized_segments))
           between_eday_sday (reduce falls-between-eday-sday characterized_segments)
           between_bday_sday (reduce falls-between-bday-sday characterized_segments)]

       (cond
         ; query date precedes first segment start date and fill_begin config is true
         (= true (< query_date first_start_day) (:fill_begin conf))
         (:classification (first characterized_segments)) ; return value of the first segment

         ; query date precedes first segment start date
         (= true (< query_date first_start_day))
         (:lc_insuff (:lc_defaults conf)) ; return lc_insuff value from lc_defaults config

         ; query date follows last segment end date and fill_end config is true
         (= true (> query_date last_end_day) (:fill_end conf))
         (:classification (last characterized_segments)) ; return value of the last segment
         
         ; query date follows last segment end date
         (= true (> query_date last_end_day))
         (:lc_insuff (:lc_defaults conf)) ; return the lc_insuff value from the lc_defaults config

         ; query date falls between a segments start date and end date
         (not (nil? intersected_segment))
         (:classification intersected_segment) ; return the class value for the intercepted model

         ; query date falls between segments of same landcover classification and fill_samelc config is true
         (= true (:fill_samelc conf) (= (:classification (first between_eday_sday)) (:classification (last between_eday_sday))))
         (:classification (last between_eday_sday)) ; return the value from the last model from the pair of models the query date fell between

         ; query date falls between one segments break date and the following segments start date and fill_difflc config is true
         (= true (:fill_difflc conf) (not (map? between_bday_sday)))
         (:classification (last between_bday_sday )) ; return the value from the last model from the pair of models the query date fell between

         ; query date falls between a segments end date and break date and fill_difflc config is true
         (= true (:fill_difflc conf) (not (nil? eday_bday_model)))
         (:classification eday_bday_model) ; return the value from the model where the query date intersected the end date and break date

         :else ; finally as a last resort return the lc_inbtw value from the configuration
         (:lc_inbtw conf)))

     (catch Exception e
       (product-exception-handler e "landcover"))))
  ([pixel_models query_date rank] ; enable passing in the configuration
   (landcover pixel_models query_date rank config)))

(defn primary-landcover
  "Return the highest landcover class value"
  [pixel_map pixel_models query_date]
  (let [predictions_valid (not-empty (filter product-specs/predictions-valid? (:predictions pixel_models)))
        value (if predictions_valid (landcover pixel_models query_date 0) (:none (:lc_map config)))]
    (hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val value)))

(defn secondary-landcover
  "Return the second highest landcover class value"
  [pixel_map pixel_models query_date]
  (let [predictions_valid (not-empty (filter product-specs/predictions-valid? (:predictions pixel_models)))
        value (if predictions_valid (landcover pixel_models query_date 1) (:none (:lc_map config)))]
    (hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val value)))

(defn annual-change
  "Return the change in landcover from the provided year, to the previous year"
  [pixel_map pixel_models query_date]
  (let [predictions_valid  (not-empty (filter product-specs/predictions-valid? (:predictions pixel_models)))
        previous_query_day (util/subtract_year query_date)
        previous_value    #(landcover pixel_models previous_query_day 0)
        latest_value      #(landcover pixel_models query_date 0)
        response_template  (hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map))]

    (cond
     ; invalid predictions, use (:none (:lc_map))
     (= false predictions_valid)
     (merge response_template {:val (:none (:lc_map config))})

     ; last class matches latest class
     (= (previous_value) (latest_value))
     (merge response_template {:val (latest_value)})

     ; classes don't match
     :else
     (merge response_template {:val (util/concat_ints (previous_value) (latest_value))}))))

(defn confidence
  "Return the landcover confidence value given the segments, probabilities, query_day and rank for a location"
  [pixel_models query_date rank]
  (try
    (let [sorted_segments (util/sort-by-key (:segments pixel_models) :sday)
          probabilities   (:predictions pixel_models)
          characterized_segments (map #(characterize-segment % query_date probabilities rank) sorted_segments)
          first_start_day   (-> (first sorted_segments) (:sday) (util/to-ordinal))
          last_end_day      (-> (last sorted_segments)  (:eday) (util/to-ordinal))
          intersected_segment (first (filter :intersects characterized_segments))
          eday_bday_model   (first (filter :btw_eday_bday characterized_segments))
          between_eday_sday (reduce falls-between-eday-sday characterized_segments)
          between_bday_sday (reduce falls-between-bday-sday characterized_segments)]

      (cond
        ; query date preceds first segment start date
        (= true (< query_date first_start_day))
        (:lcc_back (:lc_defaults config)) ; return lcc_back value from lc_defaults config

        ; query date follows last segment end date and change prob == 1
        (= true (> query_date last_end_day) (= 1 (int (:chprob (last sorted_segments)))))
        (:lcc_afterbr (:lc_defaults config)) ; return the lcc_afterbr value from the lc_defaults config

        ; query date follows last segment end date
        (= true (> query_date last_end_day))
        (:lcc_forwards (:lc_defaults config)) ; return the lcc_forwards value from the lc_defaults config

        ; query date falls between a segments start date and end date and growth is true
        (= true (not (nil? intersected_segment)) (:growth intersected_segment))
        (:lcc_growth (:lc_defaults config)) ; return lcc_growth value from lc_defaults config

        ; query date falls between a segments start date and end date and decline is true
        (= true (not (nil? intersected_segment)) (:decline intersected_segment))
        (:lcc_decline (:lc_defaults config)) ; return lcc_decline value from lc_defaults config

        ; query date falls between a segments start date and end date
        (not (nil? intersected_segment))
        (util/scale-value (nth (:prob (last (:probabilities intersected_segment))) rank))

        ; query date falls between segments of same landcover classification
        (= true (= (:classification (first between_eday_sday)) (:classification (last between_eday_sday))))
        (:lcc_samelc (:lc_defaults config)) ; return lcc_samelc from lc_defaults config

        ; query date falls between segments with different landcover classifications
        (= 2 (count between_eday_sday))
        (:lcc_difflc (:lc_defaults config)) ; return lcc_difflc from lc_defaults config

        :else ; mapify returns ValueError
        (:none (:lc_map config))))

    (catch Exception e
      (product-exception-handler e "confidence"))))

(defn primary-landcover-confidence
  "Return the landcover probability for the highest landcover class value"
  [pixel_map pixel_models query_date]
  (let [predictions_valid (not-empty (filter product-specs/predictions-valid? (:predictions pixel_models)))
        value (if predictions_valid (confidence pixel_models query_date 0) (:lcc_nomodel (:lc_defaults config)))]
    (hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val value)))

(defn secondary-landcover-confidence
  "Return the landcover probability for the 2nd highest landcover class value"
  [pixel_map pixel_models query_date]
  (let [predictions_valid (not-empty (filter product-specs/predictions-valid? (:predictions pixel_models)))
        value (if predictions_valid (confidence pixel_models query_date 1) (:lcc_nomodel (:lc_defaults config)))]
    (hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val value)))

(defn pixel-map
  "Return hash-map keyed by pixelx and pixely with a hash-map value for :segments and :predictions"
  [inputs]
  (try
    (let [pixelx      (first (:pixelxy inputs))
          pixely      (last  (:pixelxy inputs))
          segments    (get (:segments inputs)    (:pixelxy inputs)) 
          predictions (get (:predictions inputs) (:pixelxy inputs))]
      (hash-map {:px pixelx :py pixely} (hash-map :segments segments :predictions predictions)))
    (catch Exception e
      (log/errorf "Exception in products/pixel_map - pixelxy: %s exception-message: %s  stacktrace: %s " 
                  (:pixelxy inputs) (.getMessage e) (-> e stacktrace/print-stack-trace with-out-str))
      (throw (ex-info "Exception in product/pixel_map" {:source "products/pixel_map" :args-keys (keys inputs) :args-xy (:pixelxy inputs) :error-message (.getMessage e)})))))

(defn pixel-groups
  [injson]
  (let [juxt_fn (util/variable-juxt [:px :py])]
    (group-by juxt_fn injson)))

(defn flatten-values
  "Return a flat list of product values given a collection of hash-maps
  for every pixel in a chip, [{:pixely 3159045, :pixelx -2114775, :val 6290},...] ...]"
  [product_value_collection]
  (try
    (let [; group product coll by row
          row_groups (util/coll-groups product_value_collection [:pixely]) 
          ; sort row group values by pixelx ascending 
          sort-pixelx-fn (fn [i] (hash-map (:pixely (first i)) (sort-by :pixelx (last i))))
          sorted-x-vals (map sort-pixelx-fn row_groups)
          ; sort the rows by the pixely key ascending
          sorted-y-rows (sort-by (fn [i] (first (keys i))) > sorted-x-vals)
          ; finally, flatten to a one dimensional list
          flattened (util/flatten-vals sorted-y-rows :val)]
      flattened)
    (catch Exception e
      (log/errorf "Exception in products/flatten_values - input count: %s first input: %s last input: %s exception-message: %s  stacktrace: %s " 
                  (count product_value_collection) (first product_value_collection) (last product_value_collection) (.getMessage e) 
                  (-> e stacktrace/print-stack-trace with-out-str))
      (throw (ex-info "Exception in products/flatten_values" {:source "products/flatten_values" :input-count (count product_value_collection) :message (.getMessage e)})))))

(defn values
  "Returns a 1-d collection of product values"
  [segments_json predictions_json product_type queryday]
  (let [segments    (-> segments_json (keywordize-keys) (pixel-groups))
        predictions (or (some-> predictions_json (not-empty) (keywordize-keys) (pixel-groups)) [])
        product_fn  (->> product_type (product-specs/product_type_check) (str "lcmap.gaia.products/") (symbol) (resolve))
        query_ord   (-> queryday (product-specs/date_fmt_check) (util/to-ordinal))
        per_pixel_inputs (map #(pixel-map {:pixelxy % :segments segments :predictions predictions}) (keys segments))
        per_pixel_values (map #(product_fn (first (keys %)) (first (vals %)) query_ord) per_pixel_inputs)]
    (-> per_pixel_values (flatten-values) (product-specs/output_check))))

(defn chip
  [product cx cy tile query_day segments predictions]
  (try
    (let [values (values segments predictions product query_day) 
          path (ppath product cx cy tile query_day)
          data {"x" cx "y" cy "values" values}]
      {:status "success" :path path :data data :date query_day})
    (catch Exception e 
      (log/errorf "Exception in products/chip - cx: %s  cy: %s  product: %s date: %s exception-message: %s exception-data: %s" 
                  cx cy product query_day (.getMessage e) (ex-data e))
           {:status "fail" :date query_day :message (str (.getMessage e) " - " (ex-data e))})))

(defn retry-handler [i cause]
  (let [exception (::again/exception i)
        data (ex-data exception)]
    (when exception
      (do
        (if (= cause (:cause data))
          ::again/fail
          (log/infof "retrying chip: %s" data))))))

(defn generate
  [{dates :dates cx :cx cy :cy products :products tile :tile :as all}]
  (try
    (let [segments    (nemo/segments cx cy)
          predictions (nemo/predictions cx cy)
          products_dates (combo/cartesian-product products dates)
          retry_opts  {::again/callback #(retry-handler % :validation-failure) ::again/strategy (:retry_strategy config)}
          chip_again  #(again/with-retries retry_opts (chip (first %) cx cy tile (last %) segments predictions))
          results     (pmap chip_again products_dates)
          fail_filter #(filter (fn [i] (= "fail" (:status i))) %) 
          failures    (->> results fail_filter (map (fn [i] {(:date i) (:message i)})))]

      (doseq [result results]
        (when (= "success" (:status result)) 
          (log/infof "storing : %s" (get-in result [:path :name]))
          (again/with-retries (:retry_strategy config)
            (storage/put_json (:path result) (:data result)))))

      {:failures failures :products products :cx cx :cy cy :dates dates})
    (catch Exception e
      (log/errorf "Exception in products/generation ! args: %s -  message: %s - data: %s - stacktrace: %s" all (.getMessage e) (ex-data e) (-> e stacktrace/print-stack-trace with-out-str))
      (throw (ex-info "Exception in product generation" {:data (ex-data e) :error-message (.getMessage e) :args all})))))
