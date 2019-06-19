(ns lcmap.gaia.nemo
  (:require [org.httpkit.client    :as http]
            [cheshire.core         :as json]
            [clojure.tools.logging :as log]
            [lcmap.gaia.config     :refer [config]]
            [lcmap.gaia.util       :as util]))

(defn results-url
  ([x y host path]
   (str host path "?cx=" x "&cy=" y))
  ([x y path]
   (results-url x y (:nemo_host config) path)))

(defn parse_body
  [http_response]
  (json/parse-string (:body http_response)))

(defn segments
  [x y]
  (let [url (results-url x y (:segments_path config))
        options {:timeout (:nemo_timeout config)}
        response @(http/get url options)]
    (if (= 200 (:status response))
      (parse_body response)
      (do (log/debugf "Error requesting segments data from Nemo - url: %s  response: %s" url response)
          (throw (ex-info "Error requesting segments data from Nemo" {:type "data-request-error"
                                                                      :message "non-200 response from nemo for segments data"
                                                                      :status (:status response) 
                                                                      :url url}))))))

(defn predictions
  [x y]
  (let [url (results-url x y (:predictions_path config))
        options {:timeout (:nemo_timeout config)}
        response @(http/get url options)]
    (if (= 200 (:status response))
      (parse_body response)
      (do (log/debugf "Error requesting predictions data from Nemo - url: %s  response: %s" url response)
          (throw (ex-info "Error requesting predictions data from Nemo" {:type "data-request-error"
                                                                         :message "non-200 response from nemo for predictions data"
                                                                         :status (:status response) 
                                                                         :url url}))))))

(defn segments-sorted
  [x y key]
  (util/sort-by-key (segments x y) key))

