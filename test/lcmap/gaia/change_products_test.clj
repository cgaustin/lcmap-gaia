(ns lcmap.gaia.change-products-test
  (:require [clojure.test :refer :all]
            [clojure.string      :as string]
            [lcmap.gaia.change-products :as change-products]
            [lcmap.gaia.file     :as file]
            [lcmap.gaia.util     :as util]
            [lcmap.gaia.config   :refer [config]]
            [lcmap.gaia.storage  :as storage]
            [lcmap.gaia.nemo     :as nemo]
            [lcmap.gaia.test-resources :as tr]))


(def response_set (set [:px :py :values]))

(def test_inputs tr/pixel_segments)
;; lcmap.gaia.main=> (doseq [s test_inputs] (prn (format "sday: %s  eday: %s" (get s "sday") (get s "eday"))))
;; "sday: 1984-10-14  eday: 1994-06-04"
;; "sday: 1994-08-07  eday: 1997-12-21"
;; "sday: 1998-01-22  eday: 2017-09-23"

(def query_date (util/to-ordinal "1996-07-01"))


(deftest get-prefix-test-raster
  (let [grid "cu"
        date "2001-07-01"
        tile "123456"
        result (change-products/get-prefix grid date tile "raster" "time-since-change")]
    (is (= result "raster/2001/cu/123/456/time-since-change"))))

(deftest get-prefix-test-json
  (let [grid "cu"
        date "2001-07-01"
        tile "123456"
        result (change-products/get-prefix grid date tile "json" "time-since-change" 222 333)]
    (is (= result "json/2001/cu/123/456/time-since-change/222/333"))))

(deftest map-path-test
  (with-redefs [config {:region "CU" :ccd_ver "V01"}
                storage/get_url (fn [a b] (str a "/" b))]
    (let [tileid "123456"
          product "time-since-change"
          date "2007-07-01"
          result (change-products/map-path tileid product date)]
      (is (= (keys result) '(:name :prefix :url)))
      (is (= (:prefix result) "raster/2007/CU/123/456/time-since-change"))
      (is (string/includes? (:name result) "LCMAP-CU-123456-20070701-"))
      (is (string/includes? (:name result) "-V01-SCLAST.tif")))))

(deftest ppath-test
  (with-redefs [config {:region "CU"}]
    (let [product "TSC"
          x "111111"
          y "222222"
          tile "345678"
          date "2007-07-01"
          result (change-products/ppath product x y tile date)]
      (is (= result {:name "TSC-111111.0-222222.0-2007-07-01.json", :prefix "json/2007/CU/345/678/TSC/111111.0/222222.0"})))))

;; (deftest generate-test
;;   (with-redefs [nemo/segments (fn [cx cy] [1 2 3])
;;                 nemo/predictions (fn [cx cy] [4 5 6])
;;                 change-products/chip (fn [p cx cy tile day segs preds] {:path {} :status "fail" :message "bad message" :date day})]
;;     (let [input {:dates ["2006-07-01"] :cx 111111 :cy 222222 :products ["time-since-change"] :tile "012345"}
;;           result (change-products/generate input)]
;;       (is (= result {:failures '({"2006-07-01" "bad message"}), :products ["time-since-change"], :cx 111111, :cy 222222, :dates ["2006-07-01"]})))))

(deftest time-of-change-single-model-test
  (let [input (merge (second test_inputs) {"chprob" 1.0 })
        date (util/to-ordinal "1998-07-01")
        result (change-products/time-of-change input date)]
    (is (= result 22))))

(deftest time-of-change-multi-model-test
  (let [next_input (merge (second test_inputs) {"chprob" 1.0 })
        input [(first test_inputs) next_input (last test_inputs)]
        date (util/to-ordinal "1998-07-01")
        result (change-products/time-of-change [111111 222222] input date)]
    (is (= result 22))))

(deftest time-since-change-single-model-test
  (let [input (merge (second test_inputs) {"chprob" 1.0 })
        date (util/to-ordinal "1998-07-01")
        result (change-products/time-since-change input date)]
    (is (= result 160))))

(deftest time-since-change-multi-model-test
  (let [next_input (merge (second test_inputs) {"chprob" 1.0 })
        input [(first test_inputs) next_input (last test_inputs)]
        date (util/to-ordinal "1998-07-01")
        result (change-products/time-since-change [111111 222222] input date)]
    (is (= result 160))))

(deftest magnitude-of-change-single-model-test
  (let [input (merge (second test_inputs) {"chprob" 1.0 }) ; bday is 1998-01-22
        date (util/to-ordinal "1998-07-01")
        result (change-products/magnitude-of-change input date)]
    (is (= result 1530.3828972372357))))

(deftest magnitude-of-change-multi-model-test
  (let [next_input (merge (second test_inputs) {"chprob" 1.0 })
        input [(first test_inputs) next_input (last test_inputs)]
        date (util/to-ordinal "1998-07-01")
        result (change-products/magnitude-of-change [111111 222222] input date)]
    (is (= result 1530.3828972372357))))

(deftest length-of-segment-single-model-test
  (let [input (second test_inputs) ; sday is"1994-08-07", eday is "1997-12-21"
        date (util/to-ordinal "1998-07-01")
        result (change-products/length-of-segment input date)]
    (is (= result 192))))

(deftest length-of-segment-multi-model-test
  (let [date (util/to-ordinal "1998-07-01")
        result (change-products/length-of-segment [111111 222222] test_inputs date)]
    (is (= result 160))))

(deftest curve-fit-single-model-test
  (let [input (second test_inputs)
        date (util/to-ordinal "1995-07-01")
        result (change-products/curve-fit input date)]
    (is (= result 8))))

(deftest curve-fit-multi-model-test
  (let [date (util/to-ordinal "1995-07-01")
        result (change-products/curve-fit [111111 222222] test_inputs date)]
   (is (= result 8))))

