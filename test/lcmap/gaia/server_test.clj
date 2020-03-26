(ns lcmap.gaia.server-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [lcmap.gaia.raster :as raster]
            [lcmap.gaia.server :as server]
            [lcmap.gaia.test-resources :as tr]))

(deftest healthy-test
  (let [response (server/healthy {})]
    (is (= (:body response) {:healthy true}))
    (is (= (:status response) 200))))


(deftest test-failed_raster_gen
  (with-redefs [raster/create_raster (fn [i] (throw (ex-info "raster error" {:type "data-generation-error"})))]
    (let [resp (server/raster-gen {:body {:foo "bar"}})]
      (is (= 500 (:status resp)))
      (is (= "problem creating data" (:message (:body resp)))))))

