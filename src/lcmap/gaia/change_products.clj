(ns lcmap.gaia.change-products
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
  (hash-map "time-of-change"                 {:abbr "SCTIME"  :type gdal/int16}                
            "magnitude-of-change"            {:abbr "SCMAG"   :type gdal/float32}            
            "time-since-change"              {:abbr "SCLAST"  :type gdal/int16}              
            "curve-fit"                      {:abbr "SCMQA"   :type gdal/int8}                     
            "length-of-segment"              {:abbr "SCSTAB"  :type gdal/int16}))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;    CHANGE PRODUCTS    ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn product-exception-handler
  [exception product_name]
  (let [type    (keyword (str product_name "-exception"))
        message (str "Error calculating " product_name)]
    (log/errorf "%s: %s  stacktrace: %s" 
                message product_name (stacktrace/print-stack-trace exception))
    (throw (ex-info message {:type "data-generation-error" 
                             :message type 
                             :exception exception}))))

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
   (let [segments (filter product-specs/segment-valid? (:segments pixel_models))
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
   (let [segments  (filter product-specs/segment-valid? (:segments pixel_models))
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
   (let [segments (filter product-specs/segment-valid? (:segments pixel_models))
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
         segments (filter product-specs/segment-valid? (:segments pixel_models))
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
   (let [segments (filter product-specs/segment-valid? (:segments pixel_models))
         values   (map #(curve-fit % query-day) segments)
         response #(hash-map :pixelx (:px pixel_map) :pixely (:py pixel_map) :val %)]
     (if (empty? segments)
       (-> 0 (response))
       (-> (last (sort values)) (response))))))


(defn pixel-map
  "Return hash-map keyed by pixelx and pixely with a hash-map value for :segments and :predictions"
  [inputs]
  (try
    (let [pixelx      (first (:pixelxy inputs))
          pixely      (last  (:pixelxy inputs))
          segments    (get (:segments inputs)    (:pixelxy inputs))]
      (hash-map {:px pixelx :py pixely} (hash-map :segments segments)))
    (catch Exception e
      (log/errorf "Exception in products/pixel_map - pixelxy: %s message: %s  stacktrace: %s " 
                  (:pixelxy inputs) (.getMessage e) (stacktrace/print-stack-trace e))
      (throw (ex-info "Exception in product/pixel_map" {:type "data-generation-error"
                                                        :message (.getMessage e)
                                                        :arguments (keys inputs)
                                                        :pixelxy (:pixelxy inputs)})))))

(defn values
  "Returns a 1-d collection of product values"
  [segments_json product_type queryday]
  (let [segments    (-> segments_json (keywordize-keys) (util/pixel-groups))
        product_fn  (->> product_type (product-specs/product_type_check) (str "lcmap.gaia.change-products/") (symbol) (resolve))
        query_ord   (-> queryday (product-specs/date_fmt_check) (util/to-ordinal))
        per_pixel_inputs (map #(pixel-map {:pixelxy % :segments segments}) (keys segments))
        per_pixel_values (map #(product_fn (first (keys %)) (first (vals %)) query_ord) per_pixel_inputs)]
    (-> per_pixel_values (util/flatten-values) (product-specs/output_check))))

(defn chip
  [product cx cy tile query_day segments]
  (try
    (let [values (values segments product query_day) 
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
          products_dates (combo/cartesian-product products dates)
          retry_opts  {::again/callback #(retry-handler % :validation-failure) ::again/strategy (:retry_strategy config)}
          chip_again  #(again/with-retries retry_opts (chip (first %) cx cy tile (last %) segments))
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
      (log/errorf "Exception in products/generation - args: %s  message: %s  data: %s  stacktrace: %s"
                  all (.getMessage e) (ex-data e) (stacktrace/print-stack-trace e))
      (throw (ex-info "Exception in products/generate" {:type "data-generation-error"
                                                        :message (.getMessage e)
                                                        :args all})))))
