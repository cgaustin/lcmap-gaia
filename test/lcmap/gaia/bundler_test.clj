(ns lcmap.gaia.bundler-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [lcmap.gaia.bundler :as bundler]
            [lcmap.gaia.config :refer [config]]
            [lcmap.gaia.gdal :as gdal]
            [lcmap.gaia.util :as util]
            [lcmap.gaia.test-resources :as tr]))

(defn get-layer-info
  []
  (hash-map :cornerCoordinates {:upperLeft [111 333] :lowerRight [222 444]}
            :coordinateSystem {:wkt (tr/get-wkt)}))

(deftest test-download
  (with-redefs [io/input-stream (fn [i] (str i))
                io/output-stream (fn [i] (str i))
                io/copy (fn [a b] (str a "-" b))]
    (is (= "foo-bar" (bundler/download "foo" "bar")))))

(deftest test-get-metadata-values
  (with-redefs [gdal/info (fn [i] (get-layer-info))
                gdal/geographic-coords (fn [i] (list (inc (first i)) (dec (last i))))
                util/todays-year (fn [] "1984")]

    (is (= (bundler/get-metadata-values {:name "foo"} "bundle.tar" "obs.txt")
           {:pubdate "1984",
            :eastbc "223.0000000000",
            :westbc "112.0000000000",
            :bundle_name "bundle.tar",
            :southbc "443.0000000000",
            :northbc "332.0000000000",
            :observations_name "obs.txt"}))))

(deftest test-first-doy
  (is (= "2001-01-01" (bundler/first-doy "2001-07-01"))))

(deftest test-last-doy
  (is (= "2001-12-31" (bundler/last-doy "2001-07-01"))))

(deftest test-query-month
  (is (= "07" (bundler/query-month "2001-07-01"))))

(deftest test-sha512
  (is (= (bundler/sha512 "LICENSE")
         "6db610810f1b22a21ef217b4b6ace78dd5a4f427be3e6934a5770b64d019c0699459ea433b7117e955aac3feea02bd703fba2892a7961e27b2c0de859f68d7d7")))

(deftest test-sha256
   (is (= (bundler/sha256 "LICENSE")
          "88d9b4eb60579c191ec391ca04c16130572d7eedc4a86daa58bf28c6e14c9bcd")))

(deftest test-get-bundle-values
  (with-redefs [gdal/info (fn [i] (get-layer-info))
                gdal/geographic-coords (fn [i] (list (inc (first i)) (dec (last i))))
                util/todays-year (fn [] "1984")
                bundler/sha256 (fn [i] "666")
                config (merge config {:ccd_ver "01"})
                ]
    (is (= (bundler/get-bundle-values "111222" "2007-07-01" "bundle.tar" "foo.tif")
           {:standard_parallel2 "45.500000",
            :false_northing "0.000000",
            :bundle_checksum "666",
            :datum "WGS_1984",
            :tile_v "222",
            :lr_y "444.000000",
            :standard_parallel1 "29.500000",
            :production_date "20200324",
            :bundle_name "bundle.tar",
            :central_meridian "-96.000000",
            :coordinate_west "112.0000000000",
            :ul_x "111.000000",
            :region nil,
            :origin_latitude "23.000000",
            :coordinate_south "443.0000000000",
            :begin_date "2007-01-01",
            :lr_x "222.000000",
            :end_date "2007-12-31",
            :tile_h "111",
            :query_date "2007-07-01",
            :units "meters",
            :query_month "07",
            :ul_y "333.000000",
            :version "01",
            :collection "01",
            :coordinate_north "332.0000000000",
            :projection "AEA",
            :coordinate_east "223.0000000000",
            :false_easting "0.000000"}))))
