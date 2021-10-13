(ns lcmap.gaia.hawaii
  (:require [lcmap.gaia.bundler :as bundler]
            [lcmap.gaia.raster :as raster]
            [lcmap.gaia.storage :as storage]
            [clojure.string :as string]
            [environ.core :as environ]
            ;[lcmap.gaia.gdal :as gdal]
            ;[mount.core :as mount]
            [clojure.math.combinatorics :as combo]))

; metadata templates for HI https://eroslab.cr.usgs.gov/klsmith/product-metadata/-/tree/master/gaia
; bucket: ard-hi-c01-v01-aux-hi-v01-ccdc-1-0-science

; init for gdal operations, cog gen in this case
;(mount/start)
(def tiles [
            ;; {:h "000" :v "000"}
            ;; {:h "001" :v "000"}
            ;; {:h "002" :v "000"}
            ;; {:h "002" :v "001"}
            ;; {:h "003" :v "001"}
            ;; {:h "004" :v "001"}
            ;; {:h "003" :v "002"}
            {:h "004" :v "002"}])

(def years (range 2000 2021))

(def product_types ["cover" "change"])

;; user=> (f/tile-to-xy {:grid "hawaii" :dataset "ard" :tile "004002"})
;; {:x 155655.0, :y 1868895.0}

;; coords gathered from cli
;; '({"000000" {:x -444345.0, :y 2168895.0}}
;;   {"001000" {:x -294345.0, :y 2168895.0}}
;;   {"002000" {:x -144345.0, :y 2168895.0}}
;;   {"002001" {:x -144345.0, :y 2018895.0}}
;;   {"003001" {:x 5655.0, :y 2018895.0}}
;;   {"004001" {:x 155655.0, :y 2018895.0}}
;;   {"003002" {:x 5655.0, :y 1868895.0}}
;;   {"004002" {:x 155655.0, :y 1868895.0}})

; "raster/2019/CU/004/002/cover/  LCMAP_HI_004002_2019_20210722_V10_LCACHG.tif"

(defn format_prefix
  [details]
  (format "raster/%s/HI/%s/%s/%s/"
          (first details) (:h (second details)) (:v (second details)) (last details)))

; (list_bucket_contents bucket "raster/2019/CU/004/002/cover/")

(defn year-tile
  "Returns a map to be used as a key in a group-by call for a collection
   of object names"
  [object]
  (let [pieces (string/split object #"/")]
    (zipmap [:year :h :v] [(get pieces 1) (get pieces 3) (get pieces 4)])))

;(latest_tile_tifs "2005-07-01" "004002" raster/product_details)


(defn create-bundle
  [{tile :tile date :date tx :tx ty :ty :as all}]
  (let [output_names (bundler/output-names tile date)
        tiff_details  (storage/latest_tile_tifs date tile raster/product_details)
        tiff_names   (map :name tiff_details)
        xml_names    (map (fn [i] (string/replace i #".tif" ".xml")) tiff_names)
        all_names    (concat xml_names tiff_names (vals output_names))
        ;testing      (if (= (:lcmap-env config) "test") true false)
        ]

    ;(log/infof "received request to create bundle with params: %s" all)

    (try
        ; download tiffs
      (print "downloading tiffs for: %s" all)
      (bundler/get-tiffs tiff_details)

        ; validate tif compression
      (print "validating tiff compression")
      (bundler/validate-tifs tiff_details)

        ; generate layer metadata
      (print "generating layer metadata")
      (bundler/generate-layer-metadata tiff_details (:bundle output_names) (:observations output_names))

        ; generate observations list
      (print "generating observation list")
      (bundler/generate-observation-list tx ty (:observations output_names))

        ; generate cog
      (print "generating COG")
      (bundler/generate-cog tiff_details (:cog output_names))

        ; assemble bundle
      (print "assembling bundle")
      (bundler/assemble-bundle output_names tiff_names xml_names)

        ; create bundle level metadata
      (print "generating bundle metadata")
      (bundler/generate-bundle-metadata tile date (:bundle-meta output_names) (:bundle output_names) (first tiff_names))

        ; persist and clean
      ;; (if testing
      ;;   (log/infof "the env variable LCMAP_ENV='test', NOT persisting bundled products!")
      ;;   (persist-and-clean output_names tiff_details all_names))

      (merge all {:status "success" :tar (:bundle output_names)})

      (catch Exception e
        (let [msg (format "problem generating tile bundle for tile: %s date: %s, message: %s" tile date (.getMessage e))]
          (print msg)
          (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause e))))))))
    



(comment

  (:path environ/env)

  ; for setting up the repl env in a disconnected terminal
  (require '[lcmap.gaia.bundler :as bundler] '[lcmap.gaia.storage :as storage] '[lcmap.gaia.raster :as raster])
  (require '[lcmap.gaia.hawaii :refer :all])
  (require '[clojure.string :as string])
  (require '[lcmap.gaia.gdal :as gdal] '[mount.core :as mount])

  (mount/start)

  (def date "2005-07-01")
  (def tile "004002")
  (def tx 155655.0)
  (def ty 1868895.0)
  (def output_names (bundler/output-names tile date))
  (def tiff_details (storage/latest_tile_tifs date tile raster/product_details))
  (def tiff_names (map :name tiff_details))
  (def xml_names (map (fn [i] (string/replace i #".tif" ".xml")) tiff_names))
  (def all_names (concat xml_names tiff_names (vals output_names)))

  (bundler/get-tiffs tiff_details)
  (bundler/validate-tifs tiff_details)
  (bundler/generate-layer-metadata tiff_details (:bundle output_names) (:observations output_names))
  (bundler/generate-observation-list tx ty (:observations output_names))


  (def year_maps (map (fn [year] {:year year}) years))

  (def tilemaps [{:id "000000" :x -444345.0, :y 2168895.0}
                 {:id "001000" :x -294345.0, :y 2168895.0}
                 {:id "002000" :x -144345.0, :y 2168895.0}
                 {:id "002001" :x -144345.0, :y 2018895.0}
                 {:id "003001" :x 5655.0, :y 2018895.0}
                 {:id "004001" :x 155655.0, :y 2018895.0}
                 {:id "003002" :x 5655.0, :y 1868895.0}
                 {:id "004002" :x 155655.0, :y 1868895.0}])

  (defn apply_merge [i] (apply merge i))
  (def tile_year_maps (map apply_merge (combo/cartesian-product year_maps tilemaps)))

  (for [txy tile_year_maps
        :let [tile (:id txy)
              tx (:x txy)
              ty (:y txy)
              date (str (:year txy) "-07-01")]]
    ;(create-bundle {:date date :tile tile :tx tx :ty ty})
    )




  )




  