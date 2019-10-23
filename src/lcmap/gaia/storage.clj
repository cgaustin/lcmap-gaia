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

(def bucketname (:storage-bucket config))

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
     (map :key summaries)))
  ([]
   (list_bucket_contents bucketname)))

(defn list_bucket_prefixes
  ([bucket prefix]
   (:common-prefixes (s3/list-objects-v2 client-config :bucket-name bucket :prefix prefix :delimiter "/")))
  ([bucket]
   (list_bucket_prefixes bucket ""))
  ([]
   (list_bucket_prefixes bucketname)))

(defn create_bucket
  ([bucket]
   (s3/create-bucket client-config bucket))
  ([]
   (create_bucket bucketname)))

(defn put_json
  ([bucket output_path data]
   (let [encoded_data (json/encode data)
         byte_data (.getBytes encoded_data)
         byte_stream (java.io.ByteArrayInputStream. byte_data)
         metadata {:content-length (count byte_data) :content-type "application/json"}
         keyname (str (:prefix output_path) "/" (:name output_path))]

    (try
       (s3/put-object client-config :bucket-name bucket :key keyname :input-stream byte_stream :metadata metadata)
       true
       (catch Exception e
         (let [msg (format "problem putting object json %s: %s" keyname (.getMessage e))]
           (log/error msg)
           (throw (ex-info msg {:type "data-request-error" :message msg} (.getCause e))))))))
  ([output_path data]
   (put_json bucketname output_path data)))

(defn put_tiff
  ([bucket filepath filelocation]
   (try
     (let [javafile (java.io.File. filelocation)
           content-length (.length javafile)
           keyname (str (:prefix filepath) "/" (:name filepath))
           metadata {:content-length content-length :content-type "image/tiff"}]
       (s3/put-object client-config :bucket-name bucket :key keyname  :file javafile :metadata metadata)
       true)
     (catch Exception e
       (let [keyname (str (:prefix filepath) "/" (:name filepath))
             msg (format "problem putting tiff %s: %s" keyname (.getMessage e))]
         (log/error msg)
         (throw (ex-info msg {:type "data-request-error" :message msg} (.getCause e)))))))
  ([filepath filelocation]
   (put_tiff bucketname filepath filelocation)))

(defn drop_object
  ([bucket filename]
   (s3/delete-object client-config :bucket-name bucket :key filename))
  ([filename]
   (drop_object bucketname filename)))

(defn drop_bucket
  [bucket]
  (s3/delete-bucket client-config :bucket-name bucket))

(defn drop_bucket_nuclear
  [bucket]
  (while (not (empty? (list_bucket_contents bucket)))
    (do
      (doseq [object (list_bucket_contents bucket)]
        (drop_object bucket object))))
  (drop_bucket bucket)
  true)

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
   (get_json bucketname jsonpath)))

(defn set_public_acl
  ([bucket key]
   (s3/set-object-acl client-config bucket key CannedAccessControlList/PublicRead))
  ([key]
   (set_public_acl bucketname key)))

(defn get_url
  ([bucket filename expire]
   (.toString (s3/generate-presigned-url client-config bucket filename expire)))
  ([bucket filename]
   (let [today (jt/local-date)
         oneweek (jt/plus today (jt/days 7))]
     (get_url bucket filename oneweek)))
  ([filename]
   (get_url [bucketname filename])))

(defn parse_body
  [http_response]
  (json/parse-string (:body http_response)))

(defn segments
  [x y]
  (let [ix (int x)
        iy (int y)
        url (str (:endpoint client-config) "/" bucketname (:segments_path config) "/" ix "-" iy ".json")
        response (util/log-time @(http/get url) (format "storage/segments request for x %s  y %s" ix iy)) ]
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
        url (str (:endpoint client-config) "/" bucketname (:predictions_path config) "/" ix "-" iy ".json")
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

(defn tif_production_date
  "Extract production date from full object key name"
  [keyname]                                    ; raster/2009/CU/029/006/cover/<lcmap_tif>.tif 
  (let [tif (last (string/split keyname #"/")) ; LCMAP-CU-029006-2009-20190924-V01-LCPRI.tif
        elements (string/split tif #"-")]      ; ["LCMAP" "CU" "029006" "2009" "20190924" "V01" "LCPRI.tif"]
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
        name     (-> latest (string/split #"/") last)]
    (merge vals {:object-key latest :name name})))

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
        objects  (flatten (map (fn [i] (list_bucket_contents bucketname i)) prefixes))] ; collection of object keys
    (map (fn [i] (latest_tif objects i)) product_details))) ; return hash-map like product_details, but with new :object-key key/value

(defn init
  "Create bucket in object storage"
  []
  (create_bucket))

(mount/defstate bucket-init
  :start (init))
