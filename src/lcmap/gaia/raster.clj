(ns lcmap.gaia.raster
  (:require [clojure.tools.logging :as log]
            [clojure.java.io       :as io]
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
  (hash-map "primary-landcover"    {:abbr "LCPRI"   :type gdal/int8    :metadata-template "templates/lcpri_template.xml"}              
            "secondary-landcover"  {:abbr "LCSEC"   :type gdal/int8    :metadata-template "templates/lcsec_template.xml"}            
            "primary-confidence"   {:abbr "LCPCONF" :type gdal/int8    :metadata-template "templates/lcpriconf_template.xml"}  
            "secondary-confidence" {:abbr "LCSCONF" :type gdal/int8    :metadata-template "templates/lcsecconf_template.xml"} 
            "annual-change"        {:abbr "LCACHG"  :type gdal/int8    :metadata-template "templates/lcchg_template.xml"} 
            "time-of-change"       {:abbr "SCTIME"  :type gdal/int16   :metadata-template "templates/sctime_template.xml"}                
            "magnitude-of-change"  {:abbr "SCMAG"   :type gdal/float32 :metadata-template "templates/scmag_template.xml"}            
            "time-since-change"    {:abbr "SCLAST"  :type gdal/int16   :metadata-template "templates/sclast_template.xml"}              
            "curve-fit"            {:abbr "SCMQA"   :type gdal/int8    :metadata-template "templates/scmqa_template.xml"}                     
            "length-of-segment"    {:abbr "SCSTAB"  :type gdal/int16   :metadata-template "templates/scstab_template.xml"}))

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
  [tileid product_info date product]
  (let [grid      (:region config)
        repr_date (string/replace date "-" "")
        ccd_ver   (:ccd_ver config)
        data_product (first product_info)
        product_abbr (:abbr (last product_info)) 
        elements ["LCMAP" grid tileid repr_date ccd_ver product_abbr]
        name (str (string/join "-" elements) ".tif")
        prefix (storage/get-prefix grid date tileid "raster" product)
        url (storage/get_url storage/bucketname (str prefix "/" name))
        type (:type (last product_info))]
    {:name name :prefix prefix :url url :data-type type :data-product data_product}))

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

(defn add_chip_to_tile
  [name values tile_x tile_y chip_x chip_y]
  (let [[x_offset y_offset] (calc_offset tile_x tile_y chip_x chip_y)]
    (gdal/update_geotiff name values x_offset y_offset)))

(defn create_raster
  [{date :date tile :tile tilex :tilex tiley :tiley chips :chips product :product :as all}]
  (let [projection     (util/get-projection)
        product_info   (get-products product)
        rasters_detail (map #(map-details tile % date product) product_info)] ; (:name :prefix :url :data-type :data-product)

    (try
      ; create empty tiffs
      (doseq [raster rasters_detail]
        (create_blank_tile_tiff (:name raster) tilex tiley projection (:data-type raster))
        (log/infof "created empty %s raster!" (:name raster)))

      ; step thru the chips coords and retrieve the data for all products
      (doseq [chip chips
              :let [cx (:cx chip)
                    cy (:cy chip)
                    chip_path (storage/ppath product cx cy tile date)
                    chip_data (util/with-retry (storage/get_json chip_path))
                    chip_vals (fn [i] (nlcd_filter (map #(get % i) chip_data) i cx cy))]]
        
        ; for each raster product, add the appropriate data to the correct raster
        (doseq [raster rasters_detail]
          (log/infof "adding cx: %s cy: %s data raster %s" cx cy (:name raster))
          (add_chip_to_tile (:name raster) (chip_vals (:data-product raster)) tilex tiley cx cy)
          (log/infof "success for cx: %s cy: %s raster %s" cx cy (:name raster))))
     
      ; push rasters to object store
      (doseq [raster rasters_detail]
        (log/infof "pushing tiffs to object storage: %s" (:name raster))
        (storage/put_tiff raster (:name raster))
        (log/infof "deleting local tiff: %s" (:name raster))
        (io/delete-file (:name raster)))
      
      (map :url rasters_detail)

      (catch Exception e
        (let [names (seq (map :name rasters_detail))
              msg (format "problem creating rasters %s: %s" names (.getMessage e))]
          (log/error msg)
          (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause e)))))

      (finally
        (doseq [raster rasters_detail]
          (log/errorf "deleting incomplete tiff: %s" (:name raster))
          (io/delete-file (:name raster)))))))

(defn rasters-details
  [tileid date]
  (let [product_info   (merge (get-products "cover") (get-products "change"))]
    (map #(map-details tileid % date product) product_info)))
