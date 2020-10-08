(ns lcmap.gaia.storage-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [lcmap.gaia.storage :as storage]
            [lcmap.gaia.config :refer [config]]
            [lcmap.gaia.raster :refer [product_details]]
            [lcmap.gaia.test-resources :refer [objects_019014_2005]]))

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

(deftest tif_production_date-test
  (let [keyname "LCMAP_CU_029006_2009_20190924_V01_LCPRI.tif"]
    (is (= (storage/tif_production_date keyname) "20190924"))))

(deftest latest_tif-test
  (let [product_detail (first product_details)
        tif_objects (filter (fn [i] (string/includes? i ".tif")) objects_019014_2005)
        latest (storage/latest_tif tif_objects product_detail)]
    (is (= latest
           {:abbr "LCPRI"
            :type 1
            :metadata-template "templates/lcpri_template.xml"
            :object-key "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCPRI.tif"
            :name "LCMAP_CU_019014_2005_20201007_V11_LCPRI.tif"
            :url "http://localhost:7480/foo/raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCPRI.tif"}))))

(defn mock_list_bucket_contents
  [bucket prefix]
  (let [change_objects (filter #(string/includes? % "change") objects_019014_2005)
        cover_objects (filter #(string/includes? % "cover") objects_019014_2005)]
    (if (string/includes? prefix "change")
      change_objects
      cover_objects)))

(deftest latest_tile_tifs-test
  (with-redefs [config {:region "CU"}
                storage/list_bucket_contents mock_list_bucket_contents]
    (let [date "2005-07-01"
          tile "019014"
          result (storage/latest_tile_tifs date tile product_details)]
      (is (= result
             '({:abbr "LCPRI", :type 1, :metadata-template "templates/lcpri_template.xml"
                :object-key "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCPRI.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_LCPRI.tif"
                :url "/foo/raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCPRI.tif"}
               {:abbr "SCTIME", :type 2, :metadata-template "templates/sctime_template.xml"
                :object-key "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCTIME.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_SCTIME.tif"
                :url "/foo/raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCTIME.tif"}
               {:abbr "LCPCONF", :type 1, :metadata-template "templates/lcpriconf_template.xml"
                :object-key "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCPCONF.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_LCPCONF.tif"
                :url "/foo/raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCPCONF.tif"}
               {:abbr "LCACHG", :type 1, :metadata-template "templates/lcchg_template.xml"
                :object-key "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCACHG.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_LCACHG.tif"
                :url "/foo/raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCACHG.tif"}
               {:abbr "SCSTAB", :type 2, :metadata-template "templates/scstab_template.xml"
                :object-key "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCSTAB.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_SCSTAB.tif"
                :url "/foo/raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCSTAB.tif"}
               {:abbr "SCMQA", :type 1, :metadata-template "templates/scmqa_template.xml"
                :object-key "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCMQA.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_SCMQA.tif"
                :url "/foo/raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCMQA.tif"}
               {:abbr "LCSCONF", :type 1, :metadata-template "templates/lcsecconf_template.xml"
                :object-key "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCSCONF.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_LCSCONF.tif"
                :url "/foo/raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCSCONF.tif"}
               {:abbr "SCMAG", :type 6, :metadata-template "templates/scmag_template.xml"
                :object-key "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCMAG.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_SCMAG.tif"
                :url "/foo/raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCMAG.tif"}
               {:abbr "SCLAST", :type 2, :metadata-template "templates/sclast_template.xml"
                :object-key "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCLAST.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_SCLAST.tif"
                :url "/foo/raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCLAST.tif"}
               {:abbr "LCSEC", :type 1, :metadata-template "templates/lcsec_template.xml"
                :object-key "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCSEC.tif"
                :name "LCMAP_CU_019014_2005_20201007_V11_LCSEC.tif"
                :url "/foo/raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCSEC.tif"}))))))

