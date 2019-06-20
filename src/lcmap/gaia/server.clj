(ns lcmap.gaia.server
  (:require [clojure.tools.logging :as log]
            [clojure.stacktrace :as stacktrace]
            [compojure.core :as compojure]
            [compojure.route :as route]
            [ring.middleware.json :as ring-json]
            [ring.middleware.keyword-params :as ring-keyword-params]
            [ring.middleware.defaults :as ring-defaults]
            [ring.util.response :as ring-response]
            [org.httpkit.client :as http]
            [org.httpkit.server :as http-server]
            [lcmap.gaia.nemo :as nemo]
            [lcmap.gaia.file :as file]
            [lcmap.gaia.change-products :as change-products]
            [lcmap.gaia.cover-products :as cover-products]
            [lcmap.gaia.util :as util]
            [lcmap.gaia.config :refer [config]]
            [lcmap.gaia.raster :as raster]
            [lcmap.gaia.storage :as storage]
            [cheshire.core :as json]))

(defn get-configuration
  [request]
  {:status 200 :body config})

(defn raster-gen
  [{:keys [body] :as req}]
  (log/infof "Received /raster request with params: %s" (dissoc body :chips))
  (try
    (let [map_path (raster/create_raster body)]
      {:status 200 :body (assoc (dissoc body :chips) :map_name (:name map_path) :map_prefix (:prefix map_path) :map_url (:url map_path))})
    (catch Exception e
      (let [ex_data       (ex-data e)
            ex_type       (:type ex_data)
            ex_message    (:message ex_data)
            body_no_chips (dissoc body :chips)
            message       (.getMessage e)
            response      (fn [msg details] (hash-map :status 500 :body {:input body_no_chips :message msg :details details}))]
        (log/errorf "Exception in server/raster-gen ! args (minus chips): %s - message: %s - data: %s - stacktrace: %s" 
                    body_no_chips message ex_data (stacktrace/print-stack-trace e))
        (cond
         (= "data-generation-error" ex_type)
         (response "problem creating data" ex_message)

         (= "data-request-error" ex_type)
         (response "problem retrieving input data" ex_message)

         :else
         (response "problem handling this request" "contact HelpDesk"))))))

(defn raster-fetch
  [{:keys [body] :as req}]
  true)

(defmulti product-gen
  (fn [{:keys [body] :as req}]
    (keyword (:products body))))

(defmethod product-gen :cover
  [{:keys [body] :as req}]
  (try
    (let [results (cover-products/generate body)
          failures (:failures results)]

      (if (true? (empty? failures))
        {:status 200 :body (dissoc results :failures)}
        {:status 400 :body results}))
    (catch Exception e
      (let [ex_data       (ex-data e)
            ex_type       (:type ex_data)
            ex_message    (:message ex_data)
            message       (.getMessage e)
            response      (fn [msg details] (hash-map :status 500 :body {:inputs body :message msg :details details}))]
        (log/errorf "Exception in server/product-gen ! args (minus chips): %s - message: %s - data: %s - stacktrace: %s" 
                    body message ex_data (stacktrace/print-stack-trace e))
        (cond
         (= "data-generation-error" ex_type)
         (response "problem creating data" ex_message)

         (= "data-request-error" ex_type)
         (response "problem retrieving input data" ex_message)

         :else
         (response "problem handling this request" "contact HelpDesk"))))))

(defmethod product-gen :change
  [{:keys [body] :as req}]
  (try
    (let [results (change-products/generate body)
          failures (:failures results)]

      (if (true? (empty? failures))
        {:status 200 :body (dissoc results :failures)}
        {:status 400 :body results}))
    (catch Exception e
      (let [ex_data       (ex-data e)
            ex_type       (:type ex_data)
            ex_message    (:message ex_data)
            message       (.getMessage e)
            response      (fn [msg details] (hash-map :status 500 :body {:inputs body :message msg :details details}))]
        (log/errorf "Exception in server/product-gen ! args (minus chips): %s - message: %s - data: %s - stacktrace: %s" 
                    body message ex_data (stacktrace/print-stack-trace e))
        (cond
         (= "data-generation-error" ex_type)
         (response "problem creating data" ex_message)

         (= "data-request-error" ex_type)
         (response "problem retrieving input data" ex_message)

         :else
         (response "problem handling this request" "contact HelpDesk"))))))


(defn product-fetch
  [{:keys [body] :as req}]
  true)

(defn healthy
  "Handler for checking application health"
  [request]
  (log/debug "GET health")
  {:status 200 :body {:healthy true}})

(compojure/defroutes routes
  (compojure/context "/" request
    (route/resources "/")
    (compojure/GET   "/healthy" [] (healthy request))
    (compojure/GET   "/configuration" [] (get-configuration request))
    (compojure/POST  "/product"  [] (product-gen   request))
    (compojure/GET   "/product"  [] (product-fetch request))
    (compojure/POST  "/raster"   [] (raster-gen    request))
    (compojure/GET   "/raster"   [] (raster-fetch  request))))

(defn response-handler
  [routes]
  (-> routes
      (ring-json/wrap-json-body {:keywords? true})
      (ring-json/wrap-json-response)
      (ring-defaults/wrap-defaults ring-defaults/api-defaults)
      (ring-keyword-params/wrap-keyword-params)))

(def app (response-handler routes))

(defn run-server
  []
 (http-server/run-server app {:port (:http_port config)}))
