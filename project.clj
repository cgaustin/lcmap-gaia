(defproject gaia "1.4.11"
  :description "Level 2 CCDC Product Generation"
  :url "http://github.com/usgs-eros/lcmap-gaia"
  :license {:name "Unlicense"
            :url ""}
  :dependencies [[org.clojure/clojure       "1.10.1"]
                 [org.clojure/core.async    "1.3.610"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.logging "0.4.0"]
                 [org.slf4j/slf4j-log4j12 "1.7.21"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [cheshire                  "5.8.0"]
                 [clojure.java-time         "0.3.2"]
                 [org.clojure/math.combinatorics "0.1.5"]
                 [compojure                 "1.6.1"]
                 [environ                   "1.1.0"]
                 [http-kit                  "2.2.0"]
                 [http-kit.fake             "0.2.1"]
                 [ring                      "1.6.3"]
                 [ring/ring-defaults        "0.3.1"]
                 [ring/ring-json            "0.4.0"]
                 [ring/ring-jetty-adapter   "1.6.3"]
                 [ring/ring-mock            "0.3.2"]
                 [org.gdal/gdal             "2.2.0"]
                 [mount                     "0.1.12"]
                 [amazonica                 "0.3.139"]
                 [listora/again             "1.0.0"]
                 [comb                      "0.1.1"]
                 [digest                    "1.4.9"]
                 [jakarta.xml.bind/jakarta.xml.bind-api "2.3.2"]
                 [org.glassfish.jaxb/jaxb-runtime "2.3.2"]
                 ]

  :plugins [[lein-environ "1.1.0"]]
  :codox {:output-path "docs"}
  :profiles {:dev     {:plugins [[lein-codox "0.10.7"]]
                       :dependencies [[org.clojure/test.check "0.9.0"]]}
             :test    {:jvm-opts ["-Xmx2048m"]
                       :resource-paths ["test" "test/resources"]
                       :dependencies [[org.clojure/test.check "0.9.0"]]
                       :env {:storage-bucket "foo" :storage-destination "bar"}}
             :uberjar {:omit-source true
                       :aot :all}
             :repl {:env {}}}
  :repl-options {:port 8081}
  :main lcmap.gaia.main)
