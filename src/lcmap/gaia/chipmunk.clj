(ns lcmap.gaia.chipmunk
  (:import java.util.Base64)
  (:require [org.httpkit.client    :as http]
            [cheshire.core         :as json]
            [clojure.tools.logging :as log]
            [clojure.stacktrace    :as stacktrace]
            [lcmap.gaia.config     :refer [config]]
            [lcmap.gaia.util       :as util]))

(defn chips_url
  "Return the url for requesting chip data for a given ubid, x, and y"
  ([host ubid x y acquired]
   (str host "/chips?ubid=" ubid "&x=" x "&y=" y "&acquired=" acquired))
  ([ubid x y]
   (chips_url (:chipmunk_host config) ubid x y (:chipmunk_acquired config))))

(defn nlcd
  "Return decoded NLCD chip values for the given X and Y coordinates"
  [x y]
  (let [ubid "AUX_NLCD"
        url (chips_url ubid x y)
        response (util/log-time @(http/get url) (format "AUX_NLCD Chipmunk request for chip x:%s  y:%s " x y )) 
        encoded ((comp #(get % "data") first json/decode) (:body response))
        decoded (.decode (Base64/getDecoder) encoded)
        as_ints (mapv int decoded)]
    (if (= 200 (:status response))
      (mapv #(get (:nlcd_conversion config) %) as_ints)
      (do
          (log/errorf "non-200 response in chipmunk/nlcd for x %s y %s, response status: %s  body: %s" x y (:status response) (:body response))
          (throw (ex-info "Exception retrieving chipmunk/nlcd" {:type "data-request-error" 
                                                                :message (format "non-200 nlcd request response. status: %s  body: %s" (:status response) (:body response)) 
                                                                :x x 
                                                                :y y}))))))

(defn binary
  [val]
  (if (zero? val) 0 1))

(defn nlcd_mask
  "Return binary valued collection based on NLCD for masking product data"
  ([x y]
   (let [nlcd_values (nlcd x y)]
     (map binary nlcd_values)))
  ([values]
   (map binary values)))

(defn nlcd_filters
  [x y]
  (let [values (util/with-retry (nlcd x y)) 
        mask (nlcd_mask values)]
    {:mask mask :values values}))


