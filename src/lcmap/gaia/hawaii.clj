(ns lcmap.gaia.hawaii
  (:require [lcmap.gaia.bundler :as bundler]
            [lcmap.gaia.raster :as raster]
            [lcmap.gaia.storage :as storage]
            [clojure.string :as string]
            [environ.core :as environ]
            [clojure.math.combinatorics :as combo]))

(def years (map (fn [year] {:year year}) (range 2000 2021)))

(def tiles [{:id "000000" :x -444345.0, :y 2168895.0}
            {:id "001000" :x -294345.0, :y 2168895.0}
            {:id "002000" :x -144345.0, :y 2168895.0}
            {:id "002001" :x -144345.0, :y 2018895.0}
            {:id "003001" :x 5655.0,    :y 2018895.0}
            {:id "004001" :x 155655.0,  :y 2018895.0}
            {:id "003002" :x 5655.0,    :y 1868895.0}
            {:id "004002" :x 155655.0,  :y 1868895.0}])

(def tiles_years (combo/cartesian-product years tiles))

(def tile_year_maps (map (fn [i] (apply merge i)) tiles_years))

(defn create-bundle
  [{tile :tile date :date tx :tx ty :ty :as all}]
  (let [output_names (bundler/output-names tile date)
        tiff_details  (storage/latest_tile_tifs date tile raster/product_details)
        tiff_names   (map :name tiff_details)
        xml_names    (map (fn [i] (string/replace i #".tif" ".xml")) tiff_names)
        all_names    (concat xml_names tiff_names (vals output_names))]

    (try
        ; download tiffs
      (print "downloading tiffs for: %s\n" all)
      (bundler/get-tiffs tiff_details)

        ; validate tif compression
      (print "validating tiff compression\n")
      (bundler/validate-tifs tiff_details)

        ; generate layer metadata
      (print "generating layer metadata\n")
      (bundler/generate-layer-metadata tiff_details (:bundle output_names) (:observations output_names))

        ; generate observations list
      (print "generating observation list\n")
      (bundler/generate-observation-list tx ty (:observations output_names))

        ; generate cog
      (print "generating COG\n")
      (bundler/generate-cog tiff_details (:cog output_names))

        ; assemble bundle
      (print "assembling bundle\n")
      (bundler/assemble-bundle output_names tiff_names xml_names)

        ; create bundle level metadata
      (print "generating bundle metadata\n")
      (bundler/generate-bundle-metadata tile date (:bundle-meta output_names) (:bundle output_names) (first tiff_names))

        ; persist and clean
      (bundler/persist-and-clean output_names tiff_details all_names)
      
      (merge all {:status "success" :tar (:bundle output_names)})

      (catch Exception e
        (let [msg (format "problem generating tile bundle for tile: %s date: %s, message: %s" tile date (.getMessage e))]
          (print msg)
          (throw (ex-info msg {:type "data-generation-error" :message msg} (.getCause e))))))))
    

(comment

  (:path environ/env)

  ; /hsm/lsat2/[IT/ ST/]collection01/incoming/CCDC_Tile

  ; export STORAGE_LOCATION=/dropoff
  ; useradd -u ${BUNDLE_USER_ID} ${BUNDLE_USER}
  ; groupadd -g 801 lsrd_pub
  ; usermod -a -G lsrd_pub $ {BUNDLE_USER}

  ; docker run -it --entrypoint /bin/bash --name gaia_work -v $PWD:/work -v /espa-storage/downloads/lcmap/bundles:/dropoff eroslab.cr.usgs.gov:4567/lcmap/gaia:1.4.12-5c9d006

  ; for setting up the repl env in a disconnected terminal
  (require '[lcmap.gaia.bundler :as bundler] '[lcmap.gaia.storage :as storage] '[lcmap.gaia.raster :as raster])
  (require '[lcmap.gaia.hawaii :refer :all])
  (require '[clojure.string :as string])
  (require '[lcmap.gaia.gdal :as gdal] '[mount.core :as mount])

  (mount/start)

  (for [txy tile_year_maps
        :let [tile (:id txy) tx (:x txy) ty (:y txy) date (str (:year txy) "-07-01")]]
    (create-bundle {:date date :tile tile :tx tx :ty ty}))

)




  