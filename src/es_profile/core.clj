(ns es-profile.core
  (:require [org.httpkit.client :as http]
            [cemerick.url :refer [url-encode]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as async]
            [clojure.data.json :as json])
  (:gen-class))

(def sample-size 1000)

(defn query-params
  [siteId]
  {:siteId siteId
   :resultsFormat "native"
   :profile 1})

(defn ave
  [n f]
  (let [results (repeatedly n f)]
    (float (/ (reduce + results) n))))

(defn try-read-json
  [json-str]
  (try
    (json/read-str json-str)
    (catch Exception e (do (println (str "error: " (.getMessage e) ", string: " json-str))
                           {"profile" {"saluki" {"engine" 0 "total" 0}}}))))

(defn make-profile-fn
  [siteId]
  (let [url "http://localhost/api/search/search.json"
        params (map (partial query-params siteId) expModes)]
    (println "params" params)
    (fn [query]
      (let [params (map #(assoc % :q (url-encode query)) params)
            requests (map #(fn []
                             (-> @(http/get url {:query-params %})
                                  (:body)
                                  (try-read-json)
                                  (get "profile")
                                  (get "saluki")
                                  (get "engine"))) params)
            profiles (map (partial ave sample-size) requests)]
        (map (fn [p r]
               {:query query
                (:experimentalMode p) r})
             params
             profiles)))))

(defn print-result-tsv
  [[a b]]
  (let [query (:query a)
        a (get a "discrete")
        b (get b "test")]
    (println (str query \tab a \tab b))))

(defn -main
  [& args]
  (let [siteId (first args)
        queries (filter (comp not empty?) (line-seq (io/reader *in*)))
        profile-fn (make-profile-fn siteId)
        result (map profile-fn queries)]
    (println "siteId" siteId)
    (println "queries")
    (pprint queries)
    (doall (map (partial print-result-tsv) result))))

