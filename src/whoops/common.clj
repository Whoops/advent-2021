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

(defn grid-dims [grid]
  [(count (first grid)) (count grid)])

(defn grid-lookup
  ([grid point]
   (grid-lookup grid point nil))
  ([grid [x y] default]
   (let [[rows cols] (grid-dims grid)]
     (if (and (> rows x -1) (> cols y -1))
       (nth (nth grid y) x)
       default))))

(defn grid-assoc [grid [x y] val]
  (assoc-in grid [y x] val))
