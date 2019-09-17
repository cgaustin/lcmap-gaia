(ns lcmap.gaia.main
  (:gen-class)
  (:require [mount.core            :as mount]
            [clojure.tools.logging :as log]
            [lcmap.gaia.gdal       :as gdal]
            [lcmap.gaia.storage    :as storage]
            [lcmap.gaia.server     :as server]))

;; ccdc results are the number of change segments
;; detected over a 100x100 pixel area (chip)
;; There can be multiple changes over time 
;; detected per pixel.
;; if no changes are detected, a result is 
;; returned with an sday and eday == 0

(defn stderr
  [message]
  (binding [*out* *err*]
    (println message)))

(defn stdout
  [message]
  (println message))

(defn -main
  []
  (mount/start)
  (server/run-server))

