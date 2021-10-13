(ns lcmap.gaia.storage
  (:require [mount.core :as mount]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [lcmap.gaia.config :refer [config]]
            [lcmap.gaia.util :as util]
            [cheshire.core :as json]
            [org.httpkit.client :as http]
            [amazonica.aws.s3 :as s3]
            [java-time :as jt])
  (:import [com.amazonaws.services.s3.model CannedAccessControlList]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def source_bucket (:storage-bucket config))
(def dest_bucket   (:storage-destination config))

(def client-config
  {:access-key (:storage-access-key config)
   :secret-key (:storage-secret-key config)
   :endpoint   (:storage-endpoint config)
   :client-config {:path-style-access-enabled true}})

(defn get-prefix
  ([grid date tile type product]
   (let [hhh (subs tile 0 3)
         vvv (subs tile 3 6)
         year (first (string/split date #"-"))
         elements [type year grid hhh vvv product]]
     (string/join "/" elements)))
  ([grid date tile type product cx cy]
   (let [prfx (get-prefix grid date tile type product)
         elements [prfx cx cy]]
     (string/join "/" elements))))

(defn ppath
  ([product x y tile date suffix]
   (let [grid (:region config)
         fx   (util/float-string x)
         fy   (util/float-string y)
         name (->> [product fx fy date] (string/join "-") (#(str % suffix)))
         prefix (get-prefix grid date tile "json" product fx fy)]
     {:name name :prefix prefix}))
  ([product x y tile date]
   (ppath product x y tile date ".json")))

(defn list_buckets
  ([cfg]
   (let [objects (s3/list-buckets cfg)]
     (map :name objects)))
  ([]
   (list_buckets client-config)))

(defn list_bucket_contents
  ([bucket prefix]
   (let [objects (s3/list-objects-v2 client-config :bucket-name bucket :prefix prefix :delimiter "/")
         summaries (:object-summaries objects)]
     (map :key summaries)))
  ([bucket]
   (let [objects (s3/list-objects-v2 client-config :bucket-name bucket)
         summaries (:object-summaries objects)]
     (map :key summaries))))

(defn list_bucket_prefixes
  ([bucket prefix]
   (:common-prefixes (s3/list-objects-v2 client-config :bucket-name bucket :prefix prefix :delimiter "/")))
  ([bucket]
   (list_bucket_prefixes bucket "")))

(defn create_bucket
  ([bucket]
   (try (s3/create-bucket client-config bucket)
        (catch com.amazonaws.services.s3.model.AmazonS3Exception e
          (let [msg (format "problem creating bucket: %s. Ignore if not owner. Details: %s" bucket (.getMessage e))]
            (log/warn msg))))))

(defn put_json
  ([bucket output_path data]
   (let [encoded_data (json/encode data)
         byte_data (.getBytes encoded_data)
         byte_stream (java.io.ByteArrayInputStream. byte_data)
         metadata {:content-length (count byte_data) :content-type "application/json"}
         keyname (str (:prefix output_path) "/" (:name output_path))
         acl {:grant-permission ["AllUsers" "Read"]}]
     (try
       (s3/put-object client-config :bucket-name bucket :key keyname :input-stream byte_stream :metadata metadata :access-control-list acl)
       true
       (catch Exception e
         (let [msg (format "problem putting object json %s: %s" keyname (.getMessage e))]
           (log/error msg)
           (throw (ex-info msg {:type "data-request-error" :message msg} (.getCause e))))))))
  ([output_path data]
   (put_json dest_bucket output_path data)))

(defn put_tiff
  ([bucket filepath ^String filelocation]
   (try
     (let [javafile (java.io.File. filelocation)
           content-length (.length javafile)
           keyname (str (:prefix filepath) "/" (:name filepath))
           metadata {:content-length content-length :content-type "image/tiff"}
           acl {:grant-permission ["AllUsers" "Read"]}]
       (s3/put-object client-config :bucket-name bucket :key keyname  :file javafile :metadata metadata :access-control-list acl)
       true)
     (catch Exception e
       (let [keyname (str (:prefix filepath) "/" (:name filepath))
             msg (format "problem putting tiff %s: %s" keyname (.getMessage e))]
         (log/error msg)
         (throw (ex-info msg {:type "data-request-error" :message msg} (.getCause e)))))))
  ([filepath filelocation]
   (put_tiff dest_bucket filepath filelocation)))

(defn put-file
  [keyname filelocation]
  (log/infof (format "storage/put-file keyname: %s  filelocation: %s" keyname filelocation))
  (try
    (let [content_types (hash-map :tiff "image/tiff" :xml "application/xml" :json "application/json")
          type_keyword (-> filelocation (string/split #"\.") last keyword)
          javafile (java.io.File. filelocation)
          content-length (.length javafile)
          metadata {:content-length content-length :content-type (type_keyword content_types)}
          acl {:grant-permission ["AllUsers" "Read"]}]
      (s3/put-object client-config :bucket-name dest_bucket :key keyname  :file javafile :metadata metadata :access-control-list acl)
      true)
    (catch Exception e
      (let [msg (format "problem putting file %s to ceph: %s" keyname (.getMessage e))]
        (log/error msg)
        (throw (ex-info msg {:type "data-request-error" :message msg} (.getCause e)))))))

(defn drop_object
  [bucket filename]
  (s3/delete-object client-config :bucket-name bucket :key filename))

(defn drop_bucket
  [bucket]
  (s3/delete-bucket client-config :bucket-name bucket))

(defn get_json
  ([bucket jsonpath]
   (try
     (let [s3object (s3/get-object client-config :bucket-name bucket :key (str (:prefix jsonpath) "/" (:name jsonpath)))
           s3content (clojure.java.io/reader (:object-content s3object))]
       (json/parse-stream s3content))
     (catch Exception e
       (let [msg (format "problem retrieving product json %s: %s" jsonpath (.getMessage e))]
         (log/error msg)
         (throw (ex-info msg {:type "data-request-error" :message msg} (.getCause e)))))))
  ([jsonpath]
   (get_json source_bucket jsonpath)))

(defn get_url
  ([bucket keyname]
   (str (:storage-endpoint config) "/" bucket "/" keyname))
  ([keyname]
   (get_url source_bucket keyname)))

(defn set_public_acl
  ([bucket keyname]
   (s3/set-object-acl client-config bucket keyname CannedAccessControlList/PublicRead)
   (get_url bucket keyname))
  ([keyname]
   (set_public_acl source_bucket keyname)))

(defn parse_body
  [http_response]
  (json/parse-string (:body http_response)))

(defn segments
  [x y]
  (let [ix (int x)
        iy (int y)
        url (str (:endpoint client-config) "/" source_bucket (:segments_path config) "/" ix "-" iy ".json")
        response (util/log-time @(http/get url) (format "storage/segments request for x %s  y %s" ix iy))]
    (if (= 200 (:status response))
      (parse_body response)
      (do (log/debugf "Error requesting segments data from Ceph - url: %s  response: %s" url response)
          (throw (ex-info "Error requesting segments data from Ceph" {:type "data-request-error"
                                                                      :message "non-200 response from Ceph for segments data"
                                                                      :status (:status response)
                                                                      :url url}))))))

(defn predictions
  [x y]
  (let [ix (int x)
        iy (int y)
        url (str (:endpoint client-config) "/" source_bucket (:predictions_path config) "/" ix "-" iy ".json")
        response (util/log-time @(http/get url) (format "storage/predictions request for chip x:%s y:%s " ix iy))]
    (if (= 200 (:status response))
      (parse_body response)
      (do (log/debugf "Error requesting predictions data from Ceph - url: %s  response: %s" url response)
          (throw (ex-info "Error requesting predictions data from Ceph" {:type "data-request-error"
                                                                         :message "non-200 response from Ceph for predictions data"
                                                                         :status (:status response)
                                                                         :url url}))))))

(defn segments-sorted
  [x y key]
  (util/sort-by-key (segments x y) key))

(defn pixel_segments
  [x y]
  (let [segments (util/with-retry (segments-sorted x y "sday"))]
    (util/pixel-groups segments)))

(defn pixel_predictions
  [x y]
  (util/pixel-groups (predictions x y)))

(defn tif_production_date
  "Extract production date from full object key name"
  [keyname]                                    ; raster/2009/CU/029/006/cover/<lcmap_tif>.tif 
  (let [tif (last (string/split keyname #"/")) ; LCMAP_CU_029006_2009_20190924_V01_LCPRI.tif
        elements (string/split tif #"_")]      ; ["LCMAP" "CU" "029006" "2009" "20190924" "V01" "LCPRI.tif"]
    (nth elements 4)))

(defn latest_tif
  "Return the most recent product by production date"
  [tifs product_detail]
  (let [key      (key product_detail)
        vals     (val product_detail)
        abbr     (:abbr vals)
        filtered (filter (fn [i] (string/includes? i abbr)) tifs)
        sorted   (sort-by tif_production_date filtered)
        latest   (last sorted)
        name     (-> latest (string/split #"/") last)
        url      (get_url latest)]
    (merge vals {:object-key latest :name name :url url})))

(defn latest_tile_tifs
  "Return collection of most recently produced tifs from the object store
  for a given year and tile"
  [date tile product_details]
  (let [year   (first (string/split date #"-"))
        region (:region config)
        hhh    (subs tile 0 3)
        vvv    (subs tile 3 6)
        base   (format "raster/%s/%s/%s/%s/" year region hhh vvv)   ; raster/2009/CU/029/006/
        prefixes (map (fn [i] (str base i "/")) ["cover" "change"]) ; ["raster/2009/CU/029/006/cover/" ...]
        objects  (flatten (map (fn [i] (list_bucket_contents source_bucket i)) prefixes))
        tiffs  (filter (fn [i] (string/includes? i ".tif")) objects)]    ; collection of object keys
    (doall (map (fn [i] (latest_tif tiffs i)) product_details)))) ; return hash-map like product_details, but with new :object-key key/value


(comment
  (require '[lcmap.gaia.raster :as raster])
  (latest_tile_tifs "2005-07-01" "004002" raster/product_details)
  
  
  )

(defn chip
  "Return /chip data for a cx cy pair"
  [cx cy]
  (let [url (str (:endpoint client-config) "/" source_bucket "/chip/" (int cx) "-" (int cy) ".json")
        response @(http/get url)]
    (if (= 200 (:status response))
      (first (parse_body response))
      (do (log/debugf "Error requesting chip data from Ceph - url: %s  response: %s" url response)
          (throw (ex-info "Error requesting chip data from Ceph" {:type "data-request-error"
                                                                  :message "non-200 response from Ceph for chip data"
                                                                  :status (:status response)
                                                                  :url url}))))))

(defn init
  "Create bucket in object storage"
  []
  (create_bucket dest_bucket))

(mount/defstate bucket-init
  :start (init))
