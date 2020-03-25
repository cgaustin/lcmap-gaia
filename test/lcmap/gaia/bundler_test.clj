(ns lcmap.gaia.bundler-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [comb.template :as comb]
            [lcmap.gaia.bundler :as bundler]
            [lcmap.gaia.config :refer [config]]
            [lcmap.gaia.gdal :as gdal]
            [lcmap.gaia.util :as util]
            [lcmap.gaia.test-resources :as tr]
            [lcmap.gaia.storage :as storage]))

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
                util/todays-date-conc (fn [] "20200324")
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

(deftest test-get-tiffs
  (with-redefs [bundler/download (fn [url name] (spit name url))]
    (let [details [{:name "foo.txt" :url "https://eros.cr.usgs.gov"}]]
      (bundler/get-tiffs details)
      (is (= (:url (first details)) (slurp (:name (first details)))))
      ; cleanup
      (io/delete-file (:name (first details))))))

(defn exists? [f] (.exists (io/as-file f)))

(defn conditional-download
  [url name]
  ; spit the file and throw an Exception if the file doesn't exist
  ; if the file does exist, return true
  (if (exists? name)
    true
    (do
      (spit name url)
      (throw (ex-info "403 for URL" {})))))

(deftest test-get-tiffs-fix-acl
  (with-redefs [bundler/download conditional-download
                storage/set_public_acl (fn [i] (spit i "xavier"))]
    (let [detail {:name "foo.txt" :url "https://eros.cr.usgs.gov"
                  :object-key "bar.txt"}]
      (bundler/get-tiffs [detail])
      (is (exists? (:object-key detail)))
      ; cleanup
      (io/delete-file (:name detail))
      (io/delete-file (:object-key detail)))))

(deftest test-generate-layer-metadata
  (with-redefs [bundler/get-metadata-values (fn [a b c] {:foo "bar"})
                comb/eval (fn [a b] "apples and bananas")]
    (let [detail {:name "foo.tif" :metadata-template "templates/lcpri_template.xml"}]
      (bundler/generate-layer-metadata [detail] "bundle.tar" "obs.txt")
      (is (exists? "foo.xml"))
      ; cleanup
      (io/delete-file "foo.xml"))))

(deftest test-generate-bundle-metadata
  (with-redefs [bundler/get-bundle-values (fn [a b c d] {:foo "bar"})
                comb/eval (fn [a b] "oranges and pears")]
    (bundler/generate-bundle-metadata "001002" "2007-07-01" "meta.xml" "bundle.tar" "geo.tif")
    (is (exists? "meta.xml"))
    (is (= "oranges and pears" (slurp "meta.xml")))
    ; cleanup
    (io/delete-file "meta.xml")))

(deftest test-generate-cog
  (with-redefs [gdal/generate-cog spit]
    (let [tif_pre "LCMAP-CU-004002-2001-20191204-V01-"
          pri_tif (str tif_pre "LCPRI.tif")
          details [{:name pri_tif}
                   {:name (str tif_pre "SCSTAB.tif")}]]
      (bundler/generate-cog details "green eggs & ham")
      (is (exists? pri_tif))
      (is (= "green eggs & ham" (slurp pri_tif)))
      ; cleanup
      (io/delete-file pri_tif))))

(deftest test-generate-observation-list
  (with-redefs [storage/chip (fn [x y] {"dates" ["1998-07-01" "2001-07-01"]})]
    (let [obs "obs.txt"]
      (bundler/generate-observation-list 111 222 obs)
      (is (exists? obs))
      (is (= "1998-07-01\n2001-07-01" (slurp obs)))
      ; cleanup
      (io/delete-file obs))))

(deftest test-assemble-bundle
  (let [obs "obs.txt"
        bundle "bundle.tar"
        cog "cog.tif"
        tiff_names ["pri.tif" "prsec.tif"]
        meta_names ["pri.xml" "prsec.xml"]
        all_names (flatten [obs cog tiff_names meta_names])
        object_names {:observations obs :bundle bundle :cog cog}]

    ; create the files to bundle
    (doseq [n all_names] (spit n n))
    (bundler/assemble-bundle object_names tiff_names meta_names)
    (is (exists? bundle))

    ;cleanup
    (doseq [n all_names] (io/delete-file n))
    (io/delete-file bundle)))

(deftest test-push-bundle
  (with-redefs [util/copy-file spit]
    (let [names {:bundle "bundle.tar" :bundle-meta "meta.xml" :cog "cog.tif"}
          resp (bundler/push-bundle names)]

      (is (= resp "/tmp/bundle.tar"))

      (doseq [i (vals names)]
        (is (exists? i)))

      ; cleanup
      (doseq [v (vals names)]
        (io/delete-file v)))))

(deftest test-output-names
  (with-redefs [config (merge config {:region "CU" :ccd_ver "01"})
                util/todays-date-conc (fn [] "20200325")]
    (is (= (bundler/output-names "001002" "2007-07-01")
           {:observations "LCMAP_CU_001002_20200325_01_ACQS.txt"
            :bundle "LCMAP_CU_001002_2007_20200325_01_CCDC.tar"
            :bundle-meta "LCMAP_CU_001002_2007_20200325_01_CCDC.xml"
            :cog "LCMAP_CU_001002_2007_20200325_01_CCDC.tif"}))))

(deftest test-persist-metadata
  (with-redefs [storage/put-file spit]
    (let [detail {:object-key "/tmp/foo.tif"}]
      (bundler/persist-metadata [detail])
      (is (exists? "/tmp/foo.xml"))
      ; cleanup
      (io/delete-file "/tmp/foo.xml"))))
