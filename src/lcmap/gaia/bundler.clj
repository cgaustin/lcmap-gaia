(ns lcmap.gaia.bundler
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]
            [comb.template         :as comb]
            [lcmap.gaia.config     :refer [config]]
            [lcmap.gaia.file       :as file]
            [lcmap.gaia.gdal       :as gdal]
            [lcmap.gaia.nemo       :as nemo]
            [lcmap.gaia.raster     :as raster]
            [lcmap.gaia.storage    :as storage]
            [lcmap.gaia.util       :as util]))

(defn fill-template
  [name values]
  )

(defn download 
  [url name]
  (let [in (io/input-stream url)
        out (io/output-stream name)]
    (io/copy in out)))

(defn get-metadata-values
  [detail]

  (hash-map :a "b"))

(defn get-tiffs
  [tile date]
  (let [details (raster/rasters-details tile date)]

    ; download tiffs based on url in details
    (doseq [detail details]
      (log/infof (format "downloading: %s" (:name detail)))
      (download (:url detail) (:name detail)))

    ; return names
    details))

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
  [details]
  true)

(defn bundle-name
  [tile date] ; LCMAP_CU_003010_2010_20181222_V01_CCDC.tar
  true)

(defn retrieve-tiffs
  [names]
  true)

(defn generate-bundle-metadata
  [tile date tiff_names] ;LCMAP_CU_003010_2010_20181222_V01_CCDC.xml
  true)

(defn generate-cog
  [details] ; https://trac.osgeo.org/gdal/wiki/CloudOptimizedGeoTIFF
  (let [names (map :name details) 
        lcpri_name (first (filter #(re-matches #"(.*)LCPRI(.*)" %) names))
        cog_name ;LCMAP_CU_003010_2010_20181222_V01_CCDC.tif
        ]
    (gdal/generate-cog lcpri_name cog_name)
    true))

(defn generate-observation-list
  [tx ty tile date base_name]
  (let [name    (-> base_name (#(string/split % #"_")) pop (#(remove (fn [i] (= (count i) 8)) %)) (#(string/join "_" %)) (str "_ACQS.txt"))
        results (nemo/observations tx ty)
        dates   (get results "dates")
        newlined (string/join "\n" dates)]
    (spit name newlined)
    name))

(defn assemble-bundle
  [bundle_name tiff_names metadata_names cog_name observation_name]
  true)

(defn push-bundle
  [bundle_name metadata_name]
  true)

(defn output-name
  [tile date] ;LCMAP_CU_003010_2010_20181222_V01_CCDC
  (let [region   (:region config)
        ccdver   (:ccd_version config)
        year     (first (string/split date #"-"))
        today    (util/today-as-str) 
        elements ["LCMAP" region tile year today ccdver "CCDC"]]
    (string/join "_" elements)))

(defn create
  [{tile :tile tx :tx ty :ty date :date}]
  (let [output-base  (output-name tile date)
        tiff_details (get-tiffs tile date) ; downloads, returns names
        metadata     (generate-layer-metadata tiff_details) ; generates populated metadata
        observations (generate-observations-list tx ty tile date output-base)
        cog          (generate-cog tiff_details output-base)
        bundle_name (str output-base ".tar")
        ]

    (try

    ;; create bundle level metadata
    ;; (generate-bundle-metadata tile date tiff_names)

    ;; generate COG browse
    ;; (generate-bundle-cog tiff_names)

    ;; generate observation list
    ;; (generate-observation-list tx ty)

    ;; assemble bundle
    ;; (assemble-bundle tile date tiff_names)

    ;; push bundle to _________
    ;; (push-bundle tile date)

      (catch Exception e
        (let [msg (format "problem generating tile bundle for tile: %s date: %s, message: %s" tile date (.getMessage e))]
          (log/error msg)
          (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause e))))))))
