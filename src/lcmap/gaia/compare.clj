(ns lcmap.gaia.compare
  (:require [lcmap.gaia.storage :as storage]
            [lcmap.gaia.config :refer [config]]
            [lcmap.gaia.util :as util]
            [lcmap.gaia.bundler :refer [download]]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [clojure.java.shell :refer [sh]]))

;ard-cu-c01-v01-aux-cu-v01-ccdc-1-1-science/raster/2019/CU/024/013/change/LCMAP-CU-024013-2019-20200625-V01-SCLAST.tif
;ard-cu-c01-v01-aux-cu-v01-ccdc-1-1-science/raster/2019/CU/024/013/cover/LCMAP-CU-024013-2019-20200625-V01-LCPRI.tif
(def x {:tile "019014", :date "1984-07-01", :grid "conus", :ty 1214805.0, :product "cover", :tx 284415.0, :resource "raster"})
(def cxcy {:x 284415.0  :y 1214805.0})


(def years (range 1985 2020))
(def types ["change" "cover"])
(def options (cartesian-product years types))

;(def dev_bucket "ard-cu-c01-v01-aux-cu-v01-ccdc-1-1-development")
;(def dev_bucket "gaia-conf")
;(def sci_bucket "ard-cu-c01-v01-aux-cu-v01-ccdc-1-1-science")
(def dev_bucket "ard-cu-c01-v01-aux-cu-v01-ccdc-test")
(def sci_bucket "ard-cu-c01-v01-aux-cu-v01-ccdc-1-1-1-science")

(comment
  (storage/list_bucket_contents "ard-cu-c01-v01-aux-cu-v01-ccdc-test" "raster/2018/CU/020/004/change/")
'("raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCLAST.tif"
  "raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCLAST.xml"
  "raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCMAG.tif"
  "raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCMAG.xml"
  "raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCMQA.tif"
  "raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCMQA.xml"
  "raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCSTAB.tif"
  "raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCSTAB.xml"
  "raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCTIME.tif"
  "raster/2018/CU/020/004/change/LCMAP_CU_020004_2018_20210108_V11_SCTIME.xml")
  
(storage/list_bucket_contents "ard-cu-c01-v01-aux-cu-v01-ccdc-1-1-1-science" "raster/2018/CU/020/004/change/")
'("raster/2018/CU/020/004/change/LCMAP-CU-020004-2018-20210209-V01-SCLAST.tif"
  "raster/2018/CU/020/004/change/LCMAP-CU-020004-2018-20210209-V01-SCMAG.tif"
  "raster/2018/CU/020/004/change/LCMAP-CU-020004-2018-20210209-V01-SCMQA.tif"
  "raster/2018/CU/020/004/change/LCMAP-CU-020004-2018-20210209-V01-SCSTAB.tif"
  "raster/2018/CU/020/004/change/LCMAP-CU-020004-2018-20210209-V01-SCTIME.tif")


  )

(defn get_prefix
  [parts tile]
  (let [year (str (first parts))
        hhh (subs tile 0 3)
        vvv (subs tile 3 6)
        prod_type (last parts)
        elements ["raster" year "CU" hhh vvv prod_type]]
    (str (str/join "/" elements) "/")))

(defn retrieve_object
  [bucket objkey]
  (let [url (storage/get_url bucket objkey)
        dest (-> objkey (str/split #"/") last)]
    (download url dest)
    dest))

(defn compare_tiffs
  [[dev sci]]
  (let [ddev (retrieve_object dev_bucket dev)
        dsci (retrieve_object sci_bucket sci)
        result (sh "/home/cgaustin/miniconda2/envs/pygdal/bin/gdalcompare.py" ddev dsci)
        
        dmv (sh "rm" ddev)
        smv (sh "rm" dsci)]
    (str "\n" dev "\n" sci "\n" result "\n")
    ;(hash-map [dev sci] result)
    ))

(defn compare_all
  [tile]
  (let [prefixes (map #(get_prefix % tile) options)
        output (str tile "_output.txt")]
    ; for every prefix, retrieve list of objects from the sci and dev buckets
    (doseq [p prefixes
            :let [dev_objects (->> p (storage/list_bucket_contents dev_bucket) (filter #(str/includes? % ".tif")))
                  sci_objects (->> p (storage/list_bucket_contents sci_bucket) (filter #(str/includes? % ".tif")))
                  all_objects (zipmap dev_objects sci_objects)
                  results (map #(compare_tiffs %) all_objects)]]
      (prn (str "working on prefix: " p))
      (doseq [r results]
       (spit output r :append true) )
      ;(spit output (json/encode results) :append true)
      )
    
    ; for every option identify objects storage/list_bucket_contents
    ; download them
    ; run gdalcompare
   ;(doall prefixes) 
    true
    )
)

(defn compare_confs
  [tile]
  (let [xys (-> (str tile "_xys.json") slurp json/decode)]
    (doseq [xy xys
            :let []]
      
      
      )
    
    

    )
  
  
  
  )