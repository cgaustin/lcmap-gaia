(ns lcmap.gaia.bundler
  (:require [clojure.string :as string]
            [clojure.tools.logging :as log]
            [comb.template         :as template]
            [lcmap.gaia.config     :refer [config]]
            [lcmap.gaia.file       :as file]
            [lcmap.gaia.gdal       :as gdal]
            [lcmap.gaia.nemo       :as nemo]
            [lcmap.gaia.storage    :as storage]
            [lcmap.gaia.util       :as util]))

(defn fill-template
  [name values]
  )

(defn get-tiff-names
  [tile date]
  (let [rasters_detail (map #(map-details tile % date product) product_info)]


    )


  true)

(defn get-metadata-names
  [tile date]
  true)

(defn get-observations-name
  [tile date]
  true)

(defn get-cog-name
  [tile date]
  true)

(defn bundle-name
  [tile date]
  true)

(defn retrieve-tiffs
  [names]
  true)

(defn generate-layer-metadata
  [names] ;(template/eval "Hello <%= name %>" {:name "Alice"})
  true)

(defn generate-bundle-metadata
  [tile date tiff_names]
  true)

(defn generate-bundle-cog
  [tiff_names cog_name] ; https://trac.osgeo.org/gdal/wiki/CloudOptimizedGeoTIFF
  (let [lcpri_name (first (filter #(re-matches #"(.*)LCPRI(.*)" %) names))]
    (gdal/generate-cog lcpri_name cog_name)
    true))

(defn generate-observation-list
  [tx ty name]
  (let [results (nemo/observations tx ty)
        dates   (get results "dates")
        newlined (string/join "\n" dates)]
    (spit name newlined)))

(defn assemble-bundle
  [bundle_name tiff_names metadata_names cog_name observation_name]
  true)

(defn push-bundle
  [bundle_name metadata_name]
  true)

(defn create
  [{tile :tile tx :tx ty :ty date :date}]
  (let [;; for a tile + date assemble list of tif names
        tiff_names (get-tiff-names tile date)
        ;; for a tile + date assemble list of metadata names
        metadata_names (get-metadata-names tile date)
        ;; observation list name
        observations_name (get-observations-name tile date)
        ;; cog name
        cog_name (get-cog-name tile date)
        ;; calculate bundle name
        bundle_name (bundle-name tile date)
        ]

    (try
    ;; download the tiffs
    ;; (retrieve-tiffs tiff_names)

    ;; create the metadata for each tiff
    ;; (generate-layer-metadata tiff_names)

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
