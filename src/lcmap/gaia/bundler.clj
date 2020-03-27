(ns lcmap.gaia.bundler
  (:require [clojure.core.async :as async]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clojure.java.io       :as io]
            [clojure.java.shell :refer [sh]]
            [comb.template         :as comb]
            [lcmap.gaia.config     :refer [config]]
            [lcmap.gaia.file       :as file]
            [lcmap.gaia.gdal       :as gdal]
            [lcmap.gaia.raster     :as raster]
            [lcmap.gaia.storage    :as storage]
            [lcmap.gaia.util       :as util]
            [digest                :as digest]))

(defn download
  [url dest]
  (with-open [in (io/input-stream url)
              out (io/output-stream dest)]
    (io/copy in out)))

(defn get-metadata-values
  [detail bundle_name observations_name]
  ; based on lcchg_template.html
  (let [layer_info (gdal/info (:name detail))
        zp       util/zero-pad
        coord_ul (get-in layer_info [:cornerCoordinates :upperLeft])
        coord_lr (get-in layer_info [:cornerCoordinates :lowerRight])
        coord_ul_converted (gdal/geographic-coords coord_ul)
        coord_lr_converted (gdal/geographic-coords coord_lr)
        coord_west  (zp (first coord_ul_converted) 10)
        coord_east  (zp (first coord_lr_converted) 10)
        coord_north (zp (second coord_ul_converted) 10)
        coord_south (zp (second coord_lr_converted) 10)]
    (hash-map :pubdate (util/todays-year)
              :westbc  coord_west
              :eastbc  coord_east
              :northbc coord_north
              :southbc coord_south
              :bundle_name bundle_name
              :observations_name observations_name)))

(defn first-doy
  [indate]
  (let [year (first (string/split indate #"-"))]
    (string/join "-" [year "01" "01"])))

(defn last-doy
  [indate]
  (let [year (first (string/split indate #"-"))]
    (string/join "-" [year "12" "31"])))

(defn query-month
  [indate]
  (second (string/split indate #"-")))

(defn sha512
  [filename]
  (digest/sha-512 (io/as-file filename)))

(defn sha256
  [filename]
  (digest/sha-256 (io/as-file filename)))

(defn get-bundle-values
  "Return metadata values for bundle level metadata"
  [tile query_date bundle_name tif_name]
  (let [layer_info (gdal/info tif_name)
        zp       util/zero-pad
        coord_ul (get-in layer_info [:cornerCoordinates :upperLeft])
        coord_lr (get-in layer_info [:cornerCoordinates :lowerRight])
        coord_ul_converted (gdal/geographic-coords coord_ul)
        coord_lr_converted (gdal/geographic-coords coord_lr)
        coord_west (zp (first coord_ul_converted) 10)
        coord_east (zp (first coord_lr_converted) 10)
        coord_north (zp (second coord_ul_converted) 10)
        coord_south (zp (second coord_lr_converted) 10)
        hhh (subs tile 0 3)
        vvv (subs tile 3 6)
        coord_wkt (get-in layer_info [:coordinateSystem :wkt])
        coord_pattern #(re-pattern (format "(.|\n)*%s\",(.*)\\](.|\n)*" %))
        datum      (get (re-matches #"(.|\n)*DATUM\[\"(.*)\",(.|\n)*" coord_wkt) 2)
        parallel_1 (zp (get (re-matches (coord_pattern "standard_parallel_1") coord_wkt) 2) 6)
        parallel_2 (zp (get (re-matches (coord_pattern "standard_parallel_2") coord_wkt) 2) 6)
        meridian   (zp (get (re-matches (coord_pattern "longitude_of_center") coord_wkt) 2) 6)
        latitude   (zp (get (re-matches (coord_pattern "latitude_of_center") coord_wkt) 2) 6)
        easting    (zp (get (re-matches (coord_pattern "false_easting") coord_wkt) 2) 6)
        northing   (zp (get (re-matches (coord_pattern "false_northing") coord_wkt) 2) 6)]

    (hash-map :collection (:collection config)
              :version (:ccd_ver config)
              :region (:region config)
              :query_date query_date
              :begin_date (first-doy query_date)
              :end_date (last-doy query_date)
              :query_month (query-month query_date)
              :bundle_name bundle_name
              :production_date (util/todays-date-conc)
              :bundle_checksum (sha256 bundle_name)
              :coordinate_west  coord_west
              :coordinate_east  coord_east
              :coordinate_north coord_north
              :coordinate_south coord_south
              :tile_h hhh
              :tile_v vvv
              :datum datum
              :projection "AEA"
              :units "meters"
              :ul_x (zp (first coord_ul) 6)
              :ul_y (zp (second coord_ul) 6)
              :lr_x (zp (first coord_lr) 6)
              :lr_y (zp (second coord_lr) 6)
              :standard_parallel1 parallel_1
              :standard_parallel2 parallel_2
              :central_meridian meridian
              :origin_latitude latitude
              :false_easting easting
              :false_northing northing)))

(defn get-tiffs
  [details]
  ; download tiffs based on url in details
  (doseq [detail details]
    (try
      (log/infof (format "downloading: %s" (:name detail)))
      (download (:url detail) (:name detail))
      (catch Exception e
        (if (string/includes? (.getMessage e) "403 for URL")
          (do (log/infof (format "received 403 response for %s, setting acl to public read" (:url detail)))
              (storage/set_public_acl (:object-key detail))
              (download (:url detail) (:name detail)))
          (throw (ex-info (format "Error downloading tiff %s" (:url detail))
                          {:type "data-request-error" :message (.getMessage e)} (.getCause e)))))))
  details)

(defn attempt-compress
  [detail]
  (let [name (:name detail)]
    (if (gdal/compressed? name)
      (log/infof name " looks compressed")
      (do (log/infof (format "attempting to compress %s" name))
          (gdal/compress_geotiff name))))
  detail)

(defn start-consumers
  [number in-chan out-chan]
  (dotimes [_ number]
    (async/thread
      (while true
        (let [input  (async/<!! in-chan)
              result (attempt-compress input)]
          (async/>!! out-chan result))))))

(defn validate-tifs
  [details]
  (let [chunk-size (:tiff-compress-count config)
        in-chan (async/chan)
        out-chan (async/chan)
        consumers (start-consumers chunk-size in-chan out-chan)
        output_fn  (fn [i] (let [result (async/<!! out-chan)] result))]

    (async/go
      (doseq [detail details]
        (async/>! in-chan detail)))

    (doall (map output_fn details))))

(defn generate-layer-metadata
  [details bundle_name observations_name]
  (doseq [detail details
          :let [template (slurp (:metadata-template detail))
                output (string/replace (:name detail) #".tif" ".xml")
                values (get-metadata-values detail bundle_name observations_name)
                metadata (comb/eval template values)]]
    (spit output metadata))
  details)

(defn generate-bundle-metadata
  [tile date metadata_name bundle_name tiff_name] ;LCMAP_CU_003010_2010_20181222_V01_CCDC.xml
  (let [template (slurp "templates/bundle_template.xml")
        values (get-bundle-values tile date bundle_name tiff_name)
        metadata (comb/eval template values)]
    (spit metadata_name metadata)
    metadata_name))

(defn generate-cog
  [details cog_name] ; https://trac.osgeo.org/gdal/wiki/CloudOptimizedGeoTIFF
  (let [names (map :name details)
        lcpri_name (first (filter #(re-matches #"(.*)LCPRI(.*)" %) names))]
    (gdal/generate-cog lcpri_name cog_name)
    cog_name))

(defn generate-observation-list
  [tx ty name]
  (let [results (storage/chip tx ty)
        dates   (get results "dates")
        newlined (string/join "\n" dates)]
    (spit name newlined)
    name))

(defn assemble-bundle
  [object_names tiff_names metadata_names]
  (let [observations (:observations object_names)
        bundle       (:bundle object_names)
        cog          (:cog object_names)
        cmd (flatten ["tar" "-cf" bundle cog observations tiff_names metadata_names])
        result (apply sh cmd)]
    (if (= 0 (:exit result))
      bundle
      (let [msg (format "problem creating tarball %s, %s" bundle (:err result))]
        (log/errorf msg)
        (throw (ex-info msg {:type "data-generation-error" :message msg}))))))

(defn push-bundle
  [names]
  (let [bundle (:bundle names)
        meta   (:bundle-meta names)
        cog    (:cog names)
        dest   (str (:storage-location config) "/")]
    (util/copy-file bundle (str dest bundle))
    (util/copy-file meta (str dest meta))
    (util/copy-file cog (str dest cog))
    (str dest bundle)))

(defn cleanup
  [names]
  (doseq [name names]
    (util/delete name)))

(defn output-names
  [tile date]
  (let [region   (:region config)
        ccdver   (:ccd_ver config)
        year     (first (string/split date #"-"))
        year_ptn (re-pattern (str year "_"))
        today    (util/todays-date-conc)
        elements ["LCMAP" region tile year today ccdver]
        base_str (-> (string/join "_" elements) (str "_"))
        obs_str  (string/replace base_str year_ptn "")]
    (hash-map
     :observations (str obs_str  "ACQS.txt")
     :bundle       (str base_str "CCDC.tar")
     :bundle-meta  (str base_str "CCDC.xml")
     :cog          (str base_str "CCDC.tif"))))

  (defn persist-metadata
    [details]
    (doseq [detail details]
      (let [xml_keyname (string/replace (:object-key detail) #"\.tif" ".xml")
            xml_filename (last (string/split xml_keyname #"/"))]
        (storage/put-file xml_keyname xml_filename))))

  (defn persist-and-clean
    [output_names tiff_details all_names]

    (log/infof "delivering bundle")
    (push-bundle output_names)

    (log/infof "pushing generated metadata to storage")
    (persist-metadata tiff_details)

    (log/infof "cleaning up files")
    (cleanup all_names))


  (defn create
    [{tile :tile tx :tx ty :ty date :date :as all}]
    (let [output_names (output-names tile date)
          tiff_details  (storage/latest_tile_tifs date tile raster/product_details)
          tiff_names   (map :name tiff_details)
          xml_names    (map (fn [i] (string/replace i #".tif" ".xml")) tiff_names)
          all_names    (concat xml_names tiff_names (vals output_names))
          testing      (if (= (:lcmap-env config) "test") true false)]

      (log/infof "received request to create bundle with params: %s" all)

      (try
        ; download tiffs
        (log/infof "downloading tiffs for: %s" all)
        (get-tiffs tiff_details)

        ; validate tif compression
        (log/infof "validating tiff compression")
        (validate-tifs tiff_details)

        ; generate layer metadata
        (log/infof "generating layer metadata")
        (generate-layer-metadata tiff_details (:bundle output_names) (:observations output_names))

        ; generate observations list
        (log/infof "generating observation list")
        (generate-observation-list tx ty (:observations output_names))

        ; generate cog
        (log/info "generating COG")
        (generate-cog tiff_details (:cog output_names))

        ; assemble bundle
        (log/infof "assembling bundle")
        (assemble-bundle output_names tiff_names xml_names)

        ; create bundle level metadata
        (log/infof "generating bundle metadata")
        (generate-bundle-metadata tile date (:bundle-meta output_names) (:bundle output_names) (first tiff_names))

        ; persist and clean
        (if testing
          (log/infof "the env variable LCMAP_ENV='test', NOT persisting bundled products!")
          (persist-and-clean output_names tiff_details all_names))

        (merge all {:status "success" :tar (:bundle output_names)})

        (catch Exception e
          (let [msg (format "problem generating tile bundle for tile: %s date: %s, message: %s" tile date (.getMessage e))]
            (log/error msg)
            (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause e))))))))
