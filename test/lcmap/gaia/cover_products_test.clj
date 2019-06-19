(ns lcmap.gaia.cover-products-test
  (:require [clojure.test :refer :all]
            [clojure.string      :as string]
            [clojure.walk :refer [stringify-keys]]
            [clojure.math.combinatorics :as combo]
            [lcmap.gaia.cover-products :as cover-products]
            [lcmap.gaia.file     :as file]
            [lcmap.gaia.util     :as util]
            [lcmap.gaia.config   :refer [config]]
            [lcmap.gaia.storage  :as storage]
            [lcmap.gaia.nemo     :as nemo]
            [lcmap.gaia.test-resources :as tr]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;    CHANGE PRODUCT TESTS    ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def response_set (set [:pixelx :pixely :val]))

(deftest get-prefix-test-raster
  (let [grid "cu"
        date "2001-07-01"
        tile "123456"
        result (cover-products/get-prefix grid date tile "raster" "time-since-change")]
    (is (= result "raster/2001/cu/123/456/time-since-change"))))

(deftest get-prefix-test-json
  (let [grid "cu"
        date "2001-07-01"
        tile "123456"
        result (cover-products/get-prefix grid date tile "json" "time-since-change" 222 333)]
    (is (= result "json/2001/cu/123/456/time-since-change/222/333"))))


(deftest map-path-test
  (with-redefs [config {:region "CU" :ccd_ver "C01"}
                storage/get_url (fn [a b] (str a "/" b))]
    (let [tileid "123456"
          product "time-since-change"
          date "2007-07-01"
          result (cover-products/map-path tileid product date)]
      (is (= (keys result) '(:name :prefix :url)))
      (is (= (:prefix result) "raster/2007/CU/123/456/time-since-change"))
      (is (string/includes? (:name result) "LCMAP-CU-123456-20070701-"))
      (is (string/includes? (:name result) "-C01-SCLAST.tif")))))

(deftest ppath-test
  (with-redefs [config {:region "CU"}]
    (let [product "TSC"
          x "111111"
          y "222222"
          tile "345678"
          date "2007-07-01"
          result (cover-products/ppath product x y tile date)]
      (is (= result {:name "TSC-111111.0-222222.0-2007-07-01.json", :prefix "json/2007/CU/345/678/TSC/111111.0/222222.0"})))))

(deftest falls_between_eday_sday-coll-test
  (let [map_a {:follows_eday true :precedes_sday false}
        map_b {:precedes_sday true :follows_eday false}
        expected [map_a map_b]]
    (is (= (cover-products/falls-between-eday-sday map_a map_b) expected))))

(deftest falls_between_eday_sday-map-test
  (let [map_a {:follows_eday false :precedes_sday true}
        map_b {:precedes_sday true :follows_eday false}]
    (is (= (cover-products/falls-between-eday-sday map_a map_b) map_b))))

(deftest falls_between_eday_sday-nonmap-test
  (let [map_a {:follows_eday true :precedes_sday false}
        map_b {:precedes_sday true :follows_eday false}
        map_c {:precedes_sday true :follows_eday false}
        expected [map_a map_b]]
    (is (= (cover-products/falls-between-eday-sday [map_a map_b] map_c) expected))))

(deftest falls_between_bday_sday-coll-test
  (let [map_a {:follows_bday true :precedes_sday false}
        map_b {:precedes_sday true :follows_bday false}
        expected [map_a map_b]]
    (is (= (cover-products/falls-between-bday-sday map_a map_b) expected))))

(deftest nbr_test
  (let [first_seg (first tr/first_sorted_segments)
        first_sday (util/to-ordinal (get first_seg "sday"))
        first_eday (util/to-ordinal (get first_seg "eday"))
        last_seg (last tr/first_sorted_segments)
        last_sday (util/to-ordinal (get last_seg "sday"))
        last_eday (util/to-ordinal (get last_seg "eday"))
        first_nbr (cover-products/normalized-burn-ratio first_seg first_sday first_eday)
        last_nbr  (cover-products/normalized-burn-ratio last_seg last_sday last_eday)]
    (is (> first_nbr 0.12))
    (is (< first_nbr 0.14))
    (is (> last_nbr  0.33))
    (is (< last_nbr  0.34))))

(deftest get_class_test
  (let [first_class (cover-products/get-class (get (first (:predictions tr/first_segments_predictions)) "prob"))
        last_class  (cover-products/get-class (get (last (:predictions tr/first_segments_predictions)) "prob"))]
    (is (= first_class 6))
    (is (= last_class 4))))

(deftest first_date_of_class_test
  (let [sorted_predictions (util/sort-by-key (:predictions tr/first_segments_predictions) "pday")]
    (is (= "1995-07-01" (cover-products/first-date-of-class sorted_predictions 6)))
    (is (= "2012-07-01" (cover-products/first-date-of-class sorted_predictions 4)))
    (is (= nil (cover-products/first-date-of-class sorted_predictions 3)))))

(deftest mean_probabilities_test
  (let [preds [{"prob" [0 1 2 3 4 5 6 7 8]} {"prob" [7 8 9 5 8 7 6 5 5]}]]
    (is (= [3.5 4.5 5.5 4.0 6.0 6.0 6.0 6.0 6.5]
           (cover-products/mean-probabilities preds)))))

(deftest classify_positive_nbr_test
   (let [predictions tr/grass_to_forest_probs
         segments tr/first_sorted_segments
         post_forest_query_date (-> "2001-07-01" (util/to-ordinal))
         pre_forest_query_date (-> "1998-07-01" (util/to-ordinal))
         nbrdiff (float 0.06)]
     ; default value 3 is grass, 4 is tree
     (is (= 4 (cover-products/classify predictions post_forest_query_date 0 nbrdiff)))
     (is (= 3 (cover-products/classify predictions pre_forest_query_date 0 nbrdiff)))
     (is (= 3 (cover-products/classify predictions post_forest_query_date 1 nbrdiff)))
     (is (= 4 (cover-products/classify predictions pre_forest_query_date 1 nbrdiff)))
     ))

(deftest classify_negative_nbr_test
  (let [predictions tr/forest_to_grass_probs  ;(map cover-products/convert_prediction_dates tr/forest_to_grass_probs)
        segments tr/first_sorted_segments
        post_grass_query_date (-> "2001-07-01" (util/to-ordinal))
        pre_grass_query_date (-> "1998-07-01" (util/to-ordinal))
        nbrdiff (float -0.06)]
    ; default value 3 is grass, 4 is tree
    (is (= 3 (cover-products/classify predictions post_grass_query_date 0 nbrdiff)))
    (is (= 4 (cover-products/classify predictions pre_grass_query_date 0 nbrdiff)))
    (is (= 4 (cover-products/classify predictions post_grass_query_date 1 nbrdiff)))
    (is (= 3 (cover-products/classify predictions pre_grass_query_date 1 nbrdiff)))))

(deftest classify_else_test
  (let [first_segment (first tr/first_sorted_segments)
        sday (util/to-ordinal (get first_segment "sday"))
        eday (util/to-ordinal (get first_segment "eday"))
        nbrdiff (cover-products/normalized-burn-ratio first_segment sday eday) ; 0.1204...
        probs (:predictions tr/first_segments_predictions) ; (map cover-products/convert_prediction_dates (:predictions tr/first_segments_predictions))
        sorted_probabilities (util/sort-by-key probs "pday")
        ]
    (is (= 3 (cover-products/classify sorted_probabilities tr/query_ord 0 nbrdiff)))))

(deftest characterize_segment_test
  (with-redefs [cover-products/normalized-burn-ratio (fn [i x z] 66)
                cover-products/classify (fn [a b c d] 99)]
    (let [segment   {"sday" "1990-04-27" "eday" "2000-06-11" "bday" "2000-06-11" "chprob" 1.0}
          query_day (-> "1998-07-01" (util/to-ordinal))
          probabilities [{"sday" "1990-04-27" "pday" "1998-07-01"} 
                         {"sday" "1990-04-27" "pday" "2001-07-01"}]
          characterized (cover-products/characterize-segment segment query_day probabilities)]
      (is (= characterized {:intersects true
                            :precedes_sday false
                            :follows_eday false
                            :follows_bday false
                            :btw_eday_bday false
                            :sday (util/to-ordinal "1990-04-27") 
                            :eday (util/to-ordinal "2000-06-11")  
                            :bday (util/to-ordinal "2000-06-11") 
                            :growth true
                            :decline false
                            :chprob 1.0
                            :probabilities [{"sday" "1990-04-27", "pday" "1998-07-01"} {"sday" "1990-04-27" "pday" "2001-07-01"}]
                            :primary_class 99
                            :secondary_class 99})))))

(deftest landcover_test
  (let [pixel_input tr/pixel_input
        ordinal_date (-> "2001-07-01" util/to-ordinal)
        pixel_dates (combo/cartesian-product [ordinal_date] (keys pixel_input))
        characterized_pixels (map #(cover-products/characterize-inputs (last %) (get pixel_input (last %)) (first %)) pixel_dates)
        first_pixel (first characterized_pixels) ; date 730666 (2001-07-01)
        first_segment (first (:segments first_pixel)) ; bday 736594, sday 724514, eday 736594
        first_predictions (:probabilities first_segment)
        modify_segment (fn [i] (merge first_pixel {:segments [(merge first_segment i)]}))
        modify_predictions (fn [i] {:probabilities (map #(merge % i) first_predictions)}) 
        ; update cx and cy vals in probabilities, and bday val in segments
        ; (-> {"cx" 666 "cy" 777} modify_predictions (merge {:bday 666}) modify_segment)
        ]

    ; query date precedes first segment start date and fill_begin is true
    ; query date first_pixel 730666
    ; first_segment sday 724514
    ; (:fill_begin config) is true by default
    ; make query_date less than 724514
    (let [input (merge first_pixel {:date 724114})]
      (is (= (:ag (:lc_map config))
             (cover-products/landcover input 0)))
      ; query date precedes first segment start date
      (is (= (:lc_insuff (:lc_defaults config))
             (cover-products/landcover input 0 (merge config {:fill_begin false})))))

    ; query date follows last segment end date and fill_end is true
    ; first_segment eday 736594
    ; (:fill_end config) defaults true
    (let [input (merge first_pixel {:date 736894})]
      (is (= (:ag (:lc_map config))
             (cover-products/landcover input 0)))

      ; query date follows last segment end date and fill_end is false
      (is (= (:lc_insuff (:lc_defaults config))
             (cover-products/landcover input 0 (merge config {:fill_end false})))))

    ; query date falls between a segments start and end dates
    ; sday 724514, eday 736594
    ; first_segment intersects is true
    (let [input (merge first_pixel {:date 727514})]
      (is (= (:ag (:lc_map config))
             (cover-products/landcover input 0))))

    ; query date falls between segments of same landcover classification and fill_samelc config is true
    ; fill_samelc defaults to true
    (let [segment1 (merge first_segment {:eday 725000 :intersects false :follows_eday true})
          segment2 (merge first_segment {:sday 727000 :intersects false :precedes_sday true})
          input (merge first_pixel {:date 726000 :segments [segment1 segment2]})]
      (is (= (:ag (:lc_map config))
             (cover-products/landcover input 0))))

    ; query date falls between one segments break date and the following segments start date and fill_difflc config is true
    ; fill_difflc defaults true
    (let [segment1 (merge first_segment {:eday 725000 :bday 72500 :intersects false :follows_bday true})
          segment2 (merge first_segment {:sday 727000 :intersects false :precedes_sday true})
          input (merge first_pixel {:date 726000 :segments [segment1 segment2]})]
      (is (= (:ag (:lc_map config))
             (cover-products/landcover input 0 (merge config {:fill_samelc false})))))

    ; query date falls between a segments end date and break date and fill_difflc config is true
    ; first_segment: bday 736594, sday 724514, eday 736594
    ; default date 730666 (2001-07-01)
    ; fill_difflc config defaults true
    (let [segment1 (merge first_segment {:bday 736894 :intersects false :btw_eday_bday true})
          segment2 (merge first_segment {:sday 736994 :eday 737001 :bday 737001 :intersects false :precedes_sday true})
          input (merge first_pixel {:segments [segment1 segment2] :date 736694}) ; query date falls btw eday and bday of 1st segment
          mod_cfg (merge config {:fill_end false :fill_samelc false})]
      (is (= (:ag (:lc_map config))
             (cover-products/landcover input 0 mod_cfg)))

      ; as a last resort return lc_inbtw configuration value
      (is (= (:lc_inbtw config)
             (cover-products/landcover input 0 (merge mod_cfg {:fill_difflc false})))))
    ))


(deftest landcover_confidence_test ; first segment -> sday 1982-12-27 bday 2001-10-04 eday 2001-09-10
                                   ; last segment -> sday 2001-10-04  bday 2017-09-14 eday 2017-09-14
  (let [segs_probs tr/first_segments_predictions
        segs_probs_match_preds tr/first_segments_matching_predictions
        modded_segments (merge segs_probs {:segments [(first (:segments segs_probs))
                                                      (merge (last (:segments segs_probs)) {:chprob 1.0})]})
        ordinal_1995 (-> "1995-07-01" (util/to-ordinal))
        ordinal_2001 (-> "2001-09-20" (util/to-ordinal))
        ordinal_2017 (-> "2017-10-01" (util/to-ordinal))]

    ; query date precedes first segment start date and fill_begin is true
    (is (= (:lcc_back (:lc_defaults config)) 
           (cover-products/confidence segs_probs 0)))

    ; query date follows last segment end date and change prob is 1
    (is (= (:lcc_afterbr (:lc_defaults config))
           (cover-products/confidence modded_segments 0)))

     ; query date follows last segment end date and change prob is 0
    (is (= (:lcc_forwards (:lc_defaults config))
           (cover-products/confidence segs_probs 0)))

    ; query date falls between a segments start date and end date and growth is true
    (is (= (:lcc_growth (:lc_defaults config))
           (cover-products/confidence segs_probs 0)))

    ; query date falls between a segments start date and end date and decline is true
    (with-redefs [cover-products/normalized-burn-ratio (fn [i x s] -0.66)]
      (is (= (:lcc_decline (:lc_defaults config))
             (cover-products/confidence segs_probs 0))))

    ; query date falls between a segments start and end date, neither growth nor decline
    (with-redefs [cover-products/normalized-burn-ratio (fn [i x s] 0.01)]
      (is (= 8 (cover-products/confidence segs_probs 0))))

    ; query date falls between segments of same landcover classification and fill_samelc config is true
    (is (= (:lcc_samelc (:lc_defaults config))
           (cover-products/confidence segs_probs_match_preds 0)))

    ; query date falls between segments with different landcover classifications
    (is (= (:lcc_difflc (:lc_defaults config))
           (cover-products/confidence segs_probs 0)))))

;; (deftest pixel-map_test
;;   (let [inmap {:pixelxy [1 2] :segments {[1 2] [:a :b :c]} :predictions {[1 2] [:d :e :f]}}]
;;     (is (= (cover-products/pixel-map inmap) {{:px 1 :py 2} {:segments [:a :b :c] :predictions [:d :e :f]}}))))

;; (deftest pixel-groups_test
;;   (let [inmaps [{:px 1 :py 2 :foo "bar"} {:px 1 :py 2 :foo "too"} 
;;                 {:px 3 :py 4 :foo "shizzle"} {:px 3 :py 4 :foo "baz"}]
;;         value (cover-products/pixel-groups inmaps)] 
;;     (is (= (count value) 2))
;;     (is (= (keys value) '([1 2] [3 4])))))

;; (deftest flatten-values_data_test
;;   ; lower left pixel value should be the first item in returned collection
;;   (let [input [{:pixely 3159045, :pixelx -2114775, :val 2} ; lower left
;;                {:pixely 3159045, :pixelx -2114745, :val 4} ; lower right
;;                {:pixely 3159075, :pixelx -2114775, :val 6} ; upper left
;;                {:pixely 3159075, :pixelx -2114745, :val 8} ; upper right
;;                ]]
;;     (is (= (cover-products/flatten-values input)
;;            [6 8 2 4]))))

;; (deftest values_test
;;   (let [segs  tr/segments_json    ;(:segments tr/first_segments_predictions)  
;;         preds tr/predictions_json ;(:predictions tr/first_segments_predictions)
;;         first_seg (:segments tr/first_segments_predictions)
;;         first_pred (:predictions tr/first_segments_predictions)
;;         product "time-since-change"
;;         query_day "2006-07-01"]
;;     ; requesting a change product with segments and predictions is valid
;;     (is (= 2915 (nth (cover-products/values segs preds product query_day) 5)))
;;     ; cover-products/data should throw an exception when its output is too small
;;     (is (thrown-with-msg? Exception #"Validation Error" (cover-products/values first_seg first_pred product query_day)))
;;     ; requesting a landcover product with no predictions should throw an exception
;;     ;(is (thrown-with-msg? Exception #"Exception in cover-products/flatten_values" (cover-products/values segs [] "primary-landcover" query_day)))
;;     (is (= '(0) (distinct (cover-products/values segs [] "primary-landcover" query_day))))
;;     (is (= 10000 (count (cover-products/values segs [] "primary-landcover" query_day))))
;;     ; requesting a change product with no predictions is valid
;;     (is (= 2915 (nth (cover-products/values segs [] product query_day) 5)))))

(deftest product_details_test
  (is (= (keys cover-products/product_details) 
         '("primary-landcover" "annual-change" "secondary-landcover-confidence" "secondary-landcover" "primary-landcover-confidence")))
  
  (is (= (map :abbr (vals cover-products/product_details)) 
         '("LCPRI" "LCACHG" "LCSCONF" "LCSEC" "LCPCONF")))

  (is (= 5 (count cover-products/product_details))))


