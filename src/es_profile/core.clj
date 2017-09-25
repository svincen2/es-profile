(ns es-profile.core
  (:require [org.httpkit.client :as http]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as async]
            [clojure.data.json :as json])
  (:gen-class))

(defn ave
  [f n]
  (let [results (repeatedly n f)]
    (float (/ (reduce + results) n))))

; TODO - Don't hard code for a single site - parameterize
(defn profile-query
  [{:keys [name content]}]
  (let [url "http://localhost:9200/1231_read/_search"
        opt {:body (json/read-str content)}
        req #(as-> @(http/post url opt) req
               (:body req)
               (json/read-str req)
               (get req "took")
               )]
    {:name name
     :took (ave req 10)}))

(defn name-and-content
  [file]
  {:name (.getName file)
   :content (slurp file)})

(defn -main
  [& args]
  (let [dir-list (drop 1 (file-seq (io/file (first args))))
        es-queries (map name-and-content dir-list)]
    (pprint (map profile-query es-queries))))

