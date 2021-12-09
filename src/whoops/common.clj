(ns whoops.common
  (:require [clojure.string :as string]))

(defn read-input [problem]
  (let [file (str "resources/input/" problem "/input")]
    (string/split-lines (slurp file))))

(defn parse-int
  ([s]
   (Integer/parseInt s))
  ([s base]
   (Integer/parseInt s base)))

(defn parse-long
  ([s]
   (Long/parseLong s))
  ([s base]
   (Long/parseLong s base)))
