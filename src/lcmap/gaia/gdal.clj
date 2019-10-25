(ns lcmap.gaia.gdal
  (:require [mount.core :as mount]
            [clojure.java.io :as io]
            [lcmap.gaia.util :as util]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clojure.java.shell :refer [sh]])
  (:import [java.util Vector]
           [org.gdal.gdal gdal]
           [org.gdal.gdal Driver]
           [org.gdal.gdal Dataset]
           [org.gdal.gdal ColorTable]
           [org.gdal.gdalconst gdalconst]
           [org.gdal.gdalconst gdalconstJNI]))

;; init and state constructs blatantly ripped off from the
;; USGS-EROS/lcmap-chipmunk project on GitHub, created by
;; Jon Morton https://github.com/jmorton
;; 
;; ## Init
;;
;; This makes it easier to use Java GDAL libraries without
;; having to set environment variables. These are typical
;; install locations of GDAL libs on CentOS and Ubuntu.
;;
;; Before GDAL can open files, drivers must be registered.
;; Selective registration is more tedious and error prone,
;; so we just register all drivers.
;;
;; If anything goes wrong, a helpful string is printed to
;; stdout (not a log file).
;;

(defn init
  "Initialize GDAL drivers."
  []
  (try
    (util/amend-usr-path ["/usr/lib/java/gdal" "/usr/lib/jni"])
    (gdal/AllRegister)
    (catch RuntimeException e
      (binding [*out* *err*]
        (println (str "Could not update paths to native libraries. "
                      "You may need to set LD_LIBRARY_PATH to the "
                      "directory containing libgdaljni.so"))))
    (finally
      (import org.gdal.gdal.gdal))))


;; ## State
;;
;; A mount state is defined so that GDAL is initialized like
;; everything else (DB connections, HTTP listeners, etc...)
;;

(mount/defstate gdal-init
  :start (init))

; https://gdal.org/java/org/gdal/gdalconst/gdalconstConstants.html#GDT_Byte
; GDT_Byte    (1) : Eight bit unsigned integer
; GDT_Float32 (6) : Thirty two bit floating point
; GDT_UInt16  (2) : Sixteen bit unsigned integer
(def int8    (gdalconstJNI/GDT_Byte_get))
(def int16   (gdalconstJNI/GDT_UInt16_get))
(def float32 (gdalconstJNI/GDT_Float32_get))

(defn create_colortable
  [value_colors]
  (let [ct (ColorTable.)]
    (doseq [vc value_colors]
      (.SetColorEntry ct (first vc) (last vc)))
    ct))

(defn create_geotiff
  [name values ulx uly projection data_type x_size y_size x_offset y_offset colortable]
  (try
    (let [driver  (gdal/GetDriverByName "GTiff")
          options (Vector. 4)
          __add__ (.addAll options ["COMPRESS=DEFLATE" "ZLEVEL=9" "TILED=YES" "PREDICTOR=2"])
          dataset (.Create driver name x_size y_size 1 data_type options)
          band    (.GetRasterBand dataset 1)
          transform (double-array [ulx 30 0 uly 0 -30])]
      (.SetGeoTransform dataset transform)
      (.SetProjection dataset projection)
      (.WriteRaster band x_offset y_offset x_size y_size (float-array values))
      (when colortable
        (.SetRasterColorTable band colortable))
      (.delete band)
      (.delete dataset))
    name
    (catch Exception e
      (let [msg (format "problem in gdal/create_geotiff - name: %s ulx: %s uly: %s projection: %s data_type: %s x_size: %s y_size: %s x_offset: %s y_offset: %s, %s"
                        name ulx uly projection data_type x_size y_size x_offset y_offset (.getMessage e))]
        (log/errorf msg)
        (throw (ex-info msg {:type "data-generation-error"} (.getCause e)))))))

(defn update_geotiff
  ([name values x_offset y_offset x_size y_size]
   (try
     (let [dataset (gdal/Open name 1)
           band (.GetRasterBand dataset 1)]
       (.WriteRaster band x_offset y_offset x_size y_size (float-array values))
       (.delete band)
       (.delete dataset))
     (catch Exception e
       (let [msg (format "problem in gdal/update_geotiff - name: %s x_offset: %s y_offset: %s x_size: %s y_size: %s, %s"
                         name x_offset y_offset x_size y_size (.getMessage e))]
         (log/error msg)
         (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause e)))))))
  ([tiff_name values x_offset y_offset]
   (update_geotiff tiff_name values x_offset y_offset 100 100)))

(defn compress_geotiff
  [input]
  (let [command (sh "gdal_translate" input "working.tif" "-co" "COMPRESS=DEFLATE"
                                                         "-co" "ZLEVEL=9"
                                                         "-co" "TILED=YES"
                                                         "-co" "PREDICTOR=2")]
    (if (= 0 (:exit command))
      (do ; success
        (sh "rm" input)
        (sh "mv" "working.tif" input))
      (do ; fail 
        (let [msg (format "Error compressing tif: %s , message: %s" input (:err command))]
            (throw (ex-info msg {:type "data-generation-error" :message msg})))))))

(defn generate-cog
  [input output]
  (try
   (let [step1 (sh "gdal_translate" input "working.tif" "-co" "TILED=YES" "-co" "COMPRESS=DEFLATE")
         step2 (sh "gdaladdo" "-ro" "-r" "average" "working.tif" "2" "4" "8")
         step3 (sh "gdal_translate" "working.tif" output "-co" "TILED=YES" "-co" 
                   "COMPRESS=DEFLATE" "-co" "COPY_SRC_OVERVIEWS=YES" "-co" "BLOCKXSIZE=512" "-co" 
                   "BLOCKYSIZE=512" "--config" "GDAL_TIFF_OVR_BLOCKSIZE" "512")
         failed (filter (fn [i] (= 1 (:exit i))) [step1 step2 step3])]
     
     (if (empty? failed)
       output
       (let [errors (map :err failed)
             msg (format "problem with gdal utilities: %s" errors)]
         (throw (ex-info msg {:type "data-generation-error"})))))
  (catch Exception e
    (let [msg (format "problem generating COG - input: %s  output: %s, message: %s" input output (.getMessage e))]
      (log/error msg)
      (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause e)))))))

