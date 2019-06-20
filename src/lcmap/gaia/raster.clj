(ns lcmap.gaia.raster
  (:require [again.core            :as again]
            [clojure.tools.logging :as log]
            [clojure.java.io       :as io]
            [clojure.stacktrace    :as stacktrace]
            [clojure.string        :as string]
            [lcmap.gaia.config     :refer [config]]
            [lcmap.gaia.chipmunk   :as chipmunk]
            [lcmap.gaia.file       :as file]
            [lcmap.gaia.gdal       :as gdal]
            [lcmap.gaia.nemo       :as nemo]
            [lcmap.gaia.change-products :as products]
            [lcmap.gaia.storage    :as storage]
            [lcmap.gaia.util       :as util]))

;; a tile is 50 chips x 50 chips
;; each chip is 100 pixels x 100 pixels
;; each pixel is 30m x 30m
(def meters_per_pixel     30)
(def pixels_per_chip_side 100)
(def chips_per_tile_side  50)

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

(defn get-products
  [type]
  (if (= type "cover")
    (filter #(re-matches #"LC(.*)" (:abbr (last %))) product_details)
    (filter #(re-matches #"SC(.*)" (:abbr (last %))) product_details)))

(defn is-landcover
  [name]
  (let [abbr (:abbr (get product_details name))]
    (try
      (some? (re-matches #"LC(.*)" abbr))
      (catch NullPointerException e
        false))))

(defn map-details
  [tileid product_info date]
  (let [grid      (:region config)
        repr_date (string/replace date "-" "")
        ccd_ver   (:ccd_ver config)
        name      (first product_info)
        product_abbr (:abbr (last product_info)) 
        elements ["LCMAP" grid tileid repr_date ccd_ver product_abbr]
        name (str (string/join "-" elements) ".tif")
        prefix (storage/get-prefix grid date tileid "raster" product)
        url (storage/get_url storage/bucketname (str prefix "/" name))
        type (:type (last product_info))]
    {:name name :prefix prefix :url url :type type}))

(defn calc_offset
  "Returns pixel offset for UL coordinates of chips"
  [tile_x tile_y chip_x chip_y]
  (let [x_diff (- chip_x tile_x)
        y_diff (- tile_y chip_y)
        x_offset (/ x_diff meters_per_pixel)
        y_offset (/ y_diff meters_per_pixel)]
    [(int x_offset) (int y_offset)]))

(defn create_blank_tile_tiff
  [name ulx uly projection data_type]
  (let [values (repeat (* 5000 5000) 0)]
    (gdal/create_geotiff name values ulx uly projection data_type 5000 5000 0 0)))

(defn add_chip_to_tile
  [name values tile_x tile_y chip_x chip_y]
  (let [[x_offset y_offset] (calc_offset tile_x tile_y chip_x chip_y)]
    (gdal/update_geotiff name values x_offset y_offset)))

(defn nlcd_filter
  [indata product cx cy]
  (let [filters (chipmunk/nlcd_filters cx cy)
        mask    (:mask filters)
        values  (:values filters)
        fill_fn (fn [input fill] (if (zero? input) fill input))
        masked  (map * indata mask)]
    (if (is-landcover product)
      (map fill_fn masked values)
      masked)))

(defn assemble_geotiff
 [detail tx ty projection]

 (create_blank_tile_tiff (:name detail) tx ty projection (:type detail))

 (doseq [chip chips
         :let [cx (:cx chip)
               cy (:cy chip)
               chip_path (storage/ppath product cx cy tile date)
               chip_data (again/with-retries (:retry_strategy config) (storage/get_json chip_path))
               chip_vals (again/with-retries (:retry_strategy config) (nlcd_filter (get chip_data "values") product cx cy))]]
   (log/debugf "adding %s to tile: %s" (:name chip_path) tile)
   (add_chip_to_tile (:name map_path) chip_vals tilex tiley cx cy))

)

(defn create_raster
  [{date :date tile :tile tilex :tilex tiley :tiley chips :chips product :product :as all}]
  (let [projection     (util/get-projection)
        product_info   (get-products product)
        rasters_detail (map #(map-details tile date) product_info)
        geotiffs       (map #(assemble_geotiff % tx ty projection) rasters_detail)]
    (try
     

      (log/infof "pushing tiff to object storage: %s" map_path)
      (storage/put_tiff map_path (:name map_path))
      (log/infof "deleting local tiff: %s" (:name map_path))
      (io/delete-file (:name map_path))
      map_path
      (catch Exception e
        (log/errorf "Exception in raster/create_geotiff - args: %s - message: %s - data: %s - stacktrace: %s"
                    (dissoc all :chips) (.getMessage e) (ex-data e) (stacktrace/print-stack-trace e))
        (throw (ex-info "Exception in raster/create_geotiff" {:type "data-generation-error" 
                                                              :message (.getMessage e)
                                                              :map_path map_path }))))))
