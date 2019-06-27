(ns lcmap.gaia.storage-test
  (:require [clojure.test :refer :all]
            [lcmap.gaia.storage :as storage]
            [lcmap.gaia.config :refer [config]]))

(deftest get-prefix-test-raster
  (let [grid "cu"
        date "2001-07-01"
        tile "123456"
        result (storage/get-prefix grid date tile "raster" "time-since-change")]
    (is (= result "raster/2001/cu/123/456/time-since-change"))))

(deftest get-prefix-test-json
  (let [grid "cu"
        date "2001-07-01"
        tile "123456"
        result (storage/get-prefix grid date tile "json" "time-since-change" 222 333)]
    (is (= result "json/2001/cu/123/456/time-since-change/222/333"))))

(deftest ppath-test
  (with-redefs [config {:region "CU"}]
    (let [product "TSC"
          x "111111"
          y "222222"
          tile "345678"
          date "2007-07-01"
          result (storage/ppath product x y tile date)]
      (is (= result {:name "TSC-111111.0-222222.0-2007-07-01.json", :prefix "json/2007/CU/345/678/TSC/111111.0/222222.0"})))))

