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

(def query_date (util/to-ordinal "1996-07-01"))
(def test_inputs tr/pixel_segments)
;; lcmap.gaia.main=> (doseq [s tr/pixel_segments] (prn (format "sday: %s  eday: %s" (get s "sday") (get s "eday"))))
;; "sday: 1984-10-14  eday: 1994-06-04"
;; "sday: 1994-08-07  eday: 1997-12-21"
;; "sday: 1998-01-22  eday: 2017-09-23"


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

(deftest generate-test
  (with-redefs [nemo/segments-sorted (fn [a b c] (util/sort-by-key tr/segments_json "sday"))
                storage/put_json (fn [a b] true)]
    (let [params {:dates ["2007-07-01" "2008-07-01"] :cx 111111 :cy 222222 :tile "123456"}
          result (change-products/generate params)]
      (is (= result {:products "change" :cx 111111 :cy 222222 :dates ["2007-07-01" "2008-07-01"] :pixels 20000})))))
