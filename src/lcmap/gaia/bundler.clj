(ns lcmap.gaia.bundler
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clojure.java.io       :as io]
            [comb.template         :as comb]
            [lcmap.gaia.config     :refer [config]]
            [lcmap.gaia.file       :as file]
            [lcmap.gaia.gdal       :as gdal]
            [lcmap.gaia.nemo       :as nemo]
            [lcmap.gaia.raster     :as raster]
            [lcmap.gaia.storage    :as storage]
            [lcmap.gaia.util       :as util]))

(defn download 
  [url name]
  (let [in (io/input-stream url)
        out (io/output-stream name)]
    (io/copy in out)))

(defn get-metadata-values
  [detail]

  (hash-map :a "b"))

(defn get-tiffs
  [details]
  ; download tiffs based on url in details
  (doseq [detail details]
    (log/infof (format "downloading: %s" (:name detail)))
    (download (:url detail) (:name detail)))
  details)

(defn generate-layer-metadata
  [details]
  (doseq [detail details
          :let [template (slurp (:metadata-template detail))          ; read in template
                output (string/replace (:name detail) #".tif" ".xml") ; calc output name
                values (get-metadata-values detail)                   ; calc sub values
                metadata (comb/eval template values)]]                ; sub in values ;(comb/eval "Hello <%= name %>" {:name "Alice"})
    ; write out file
    (spit output metadata))
  details)

(defn generate-bundle-metadata
  [tile date name] ;LCMAP_CU_003010_2010_20181222_V01_CCDC.xml
  true)

(defn generate-cog
  [details cog_name] ; https://trac.osgeo.org/gdal/wiki/CloudOptimizedGeoTIFF
  (let [names (map :name details) 
        lcpri_name (first (filter #(re-matches #"(.*)LCPRI(.*)" %) names))]
    (gdal/generate-cog lcpri_name cog_name)
    cog_name))

(defn generate-observation-list
  [tx ty tile date name]
  (let [results (nemo/observations tx ty)
        dates   (get results "dates")
        newlined (string/join "\n" dates)]
    (spit name newlined)
    name))

(defn assemble-bundle
  [bundle_name tiff_names metadata_names cog_name observation_name]
  true)

(defn push-bundle
  [names]
  (let [bundle (:bundle names)
        meta   (:bundle-meta names)]

    )
  true)

(defn output-names
  [tile date]
  (let [region   (:region config)
        ccdver   (:ccd_version config)
        year     (first (string/split date #"-"))
        today    (util/today-as-str) 
        elements ["LCMAP" region tile year today ccdver]
        base_str (string/join "_" elements)
        obs_str  (string/replace base_str (re-pattern (str year "_")) "")]
    (hash-map
     :observations (str obs_str "ACQS.txt")
     :bundle       (str base_str "CCDC.tar")
     :bundle-meta  (str base_str "CCDC.xml")
     :cog          (str base_str "CCDC.tif"))))

(defn create
  [{tile :tile tx :tx ty :ty date :date}]
  (let [output_names (output-names tile date)
        tiff_details (raster/rasters-details tile date)]

    (try
      ; download tiffs
      (get-tiffs tiff_details)
      ; generate layer metadata
      (generate-layer-metadata tiff_details)
      ; generate observations list
      (generate-observation-list tx ty tile date (:observations output_names))
      ; generate cog
      (generate-cog tiff_details (:cog output_names))
      ; create bundle level metadata
      (generate-bundle-metadata tile date (:bundle-meta output_names))
      ; assemble bundle
      (assemble-bundle tile date tiff_details output_names)
      ; store bundle
      (push-bundle output_names)

      (catch Exception e
        (let [msg (format "problem generating tile bundle for tile: %s date: %s, message: %s" tile date (.getMessage e))]
          (log/error msg)
          (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause e))))))))
