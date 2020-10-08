(ns lcmap.gaia.test-resources
  (:require [lcmap.gaia.file :as file]
            [lcmap.gaia.util :as util]
            [lcmap.gaia.cover-products :as cp]
            [cheshire.core   :as json]
            [clojure.walk :refer [keywordize-keys stringify-keys]]
            [clojure.math.combinatorics :as combo]))

(defn ccdc_map
  [inputs]
  (let [segments    (get (:segments inputs)    (:pixelxy inputs))
        predictions (get (:predictions inputs) (:pixelxy inputs))]
    (hash-map {:px (first (:pixelxy inputs)) :py (last (:pixelxy inputs))}
              {:segments segments :predictions predictions})))

(def first_pixel_input (json/decode (slurp "resources/first_pixel_input.json")))
(def pixel_input (hash-map (first first_pixel_input) {:segments (get (last first_pixel_input) "segments") :predictions (get (last first_pixel_input) "predictions")}))
(def pixel_segments (json/decode (slurp "resources/pixel_1635765_2065695_segments.json")))

(def query_ord (util/to-ordinal "2006-07-01"))

(def segments_json (file/read-json "resources/cx-2115585_cy3119805_segment.json"))
(def predictions_json (file/read-json "resources/cx-2115585_cy3119805_prediction_with_fake_date.json"))

(def grouped_segments    (-> ["px" "py"] (util/variable-juxt) (group-by segments_json)))
(def grouped_predictions (-> ["px" "py"] (util/variable-juxt) (group-by predictions_json)))
(def pixel_map (map #(ccdc_map {:pixelxy % :segments grouped_segments :predictions grouped_predictions}) (keys grouped_segments)))

(def first_segments_predictions (-> (first pixel_map) (vals) (first)))
(def first_sorted_segments (util/sort-by-key (:segments first_segments_predictions) "sday"))

(def first_prob  {"cx" -2115585, "cy" 3119805, "px" -2114685, "py" 3118215, "sday" "1982-12-27", "eday" "2001-09-10", "pday" "1995-07-01"})
(def last_prob   {"cx" -2115585, "cy" 3119805, "px" -2114685, "py" 3118215, "sday" "1982-12-27", "eday" "2001-09-10", "pday" "2000-07-01"})
(def forest_prob {"prob" [0.082641214 0.23013946 1.6417161E-8 3.1727698E-9 0.7288632 7.0137938E-4 0.35765457 6.6413847E-8 6.6413847E-8]})
(def grass_prob  {"prob" [6.6413847E-8 6.6413847E-8 0.35765457 0.7137938 0.082641214 3.1727698E-9 1.6417161E-8 0.3288632 0.23013946]})
(def forest_to_grass_probs [(merge first_prob forest_prob) (merge last_prob grass_prob)])
(def grass_to_forest_probs [(merge first_prob grass_prob) (merge last_prob forest_prob)])
(def first_segments_matching_predictions (merge first_segments_predictions {:predictions [(merge first_prob forest_prob)
                                                                                          (merge last_prob {"sday" "2001-10-04", "eday" "2017-09-14", "pday" "2017-09-14"} forest_prob)]}))

(defn cover_data_1086765_1975335
  ([date px py]
  (let [predictions (json/decode (slurp "resources/1085415_1976805_predictions.json"))
        segments    (json/decode (slurp "resources/1085415_1976805_segments.json"))
        grouped_segments     (util/pixel-groups segments)
        grouped_predictions  (util/pixel-groups predictions)
        pixel_coords         (keys grouped_segments)
        pixel_hash-map       #(hash-map % {:segments (get grouped_segments %) :predictions (get grouped_predictions %)})
        pixel_inputs         (into {} (map pixel_hash-map pixel_coords))
        ordinal_date         (util/to-ordinal date)
        pixel_dates      (combo/cartesian-product [ordinal_date] (keys pixel_inputs))
        characterized    (map #(cp/characterize-inputs (last %) (get pixel_inputs (last %)) (first %)) pixel_dates)
        pixel_products   (map #(cp/products % ordinal_date) characterized)]
    (filter (fn [i] (and (= px (:px i)) (= py (:py i)))) pixel_products)))
  ([]
   (cover_data_1086765_1975335 "2010-07-01" 1086765 1975335)))

(defn get-wkt
   []
   "PROJCS[\"Albers\",\n
    GEOGCS[\"WGS 84\",\n
    DATUM[\"WGS_1984\",\n
    SPHEROID[\"WGS 84\",6378140,298.2569999999957,\n
    AUTHORITY[\"EPSG\",\"7030\"]],\n
    AUTHORITY[\"EPSG\",\"6326\"]],\n
    PRIMEM[\"Greenwich\",0],\n
    UNIT[\"degree\",0.0174532925199433],\n
    AUTHORITY[\"EPSG\",\"4326\"]],\n
    PROJECTION[\"Albers_Conic_Equal_Area\"],\n
    PARAMETER[\"standard_parallel_1\",29.5],\n
    PARAMETER[\"standard_parallel_2\",45.5],\n
    PARAMETER[\"latitude_of_center\",23],\n
    PARAMETER[\"longitude_of_center\",-96],\n
    PARAMETER[\"false_easting\",0],\n
    PARAMETER[\"false_northing\",0],\n
    UNIT[\"metre\",1,\n
    AUTHORITY[\"EPSG\",\"9001\"]]]")

(def objects_019014_2005
  '("raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCACHG.tif"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCACHG.xml"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCPCONF.tif"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCPCONF.xml"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCPRI.tif"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCPRI.xml"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCSCONF.tif"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCSCONF.xml"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCSEC.tif"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201006_V01_LCSEC.xml"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCACHG.tif"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCPCONF.tif"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCPRI.tif"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCSCONF.tif"
    "raster/2005/CU/019/014/cover/LCMAP_CU_019014_2005_20201007_V11_LCSEC.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCLAST.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCLAST.xml"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCMAG.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCMAG.xml"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCMQA.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCMQA.xml"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCSTAB.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCSTAB.xml"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCTIME.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201006_V01_SCTIME.xml"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCLAST.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCMAG.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCMQA.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCSTAB.tif"
    "raster/2005/CU/019/014/change/LCMAP_CU_019014_2005_20201007_V11_SCTIME.tif"))
