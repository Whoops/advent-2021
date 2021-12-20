(ns whoops.p20
  (:require [whoops.common :refer [read-input parse-long grid-dims grid-assoc grid-lookup]]
            [clojure.string :as string]))


(defn parse-input [input]
  (let [decode-pos #(if (= \# %) 1 0)
        decoder (mapv decode-pos (first input))]
    [decoder (->> (drop 2 input)
                  (mapv #(mapv decode-pos %)))]))

(def day20-data (parse-input (read-input "20-1")))

(defn expand-grid [grid size default]
  (let [[_rows cols] (grid-dims grid)
        new-rows (vec (repeat size (vec (repeat (+ (* 2 size) cols) default))))
        new-cols (vec (repeat size default))
        update-row #(vec (concat new-cols % new-cols))]
    (vec (concat new-rows
                 (mapv update-row grid)
                 new-rows))))

(defn surrounding [[x y]]
  (for [j (range (dec y) (+ 2 y))
        i (range (dec x) (+ 2 x))]
    [i j]))

(defn point->val [grid point default]
  (->> point
       surrounding
       (map #(grid-lookup grid % default))
       string/join
       (#(parse-long % 2))))

(defn decode-val [decoder val]
  (nth decoder val))

(defn decode-point [grid decoder point default]
  (as-> point $
       (point->val grid $ default)
       (decode-val decoder $)))

(defn decode-grid [decoder grid default]
  (let [new-grid (expand-grid grid 1 default)
        [rows cols] (grid-dims new-grid)]
    (mapv (fn [y]
            (mapv (fn [x]
                    (decode-point new-grid decoder [x y] default)) (range rows))) (range cols))))


(defn decode-times [decoder grid times]
  (loop [default 0
         grid grid
         cnt 0]
    (if (= cnt times)
      grid
      (let [new-grid (decode-grid decoder grid default)
            new-default (if (and (= 1 (first decoder)) (zero? default)) 1 0)]
        (recur new-default new-grid (inc cnt))))))
;; 5363 - too high
;; 5155 - too low

(defn draw-grid [grid]
  (let [translate-char #(if (= 1 %) \# \.)]
    (->> grid
         (map #(map translate-char %))
         (map string/join)
         (string/join "\n")
         println)))

(defn p20-1 []
  (let [[decoder grid] day20-data]
    (->> (decode-times decoder grid 2)
         (map #(reduce + %))
         (reduce +))))

(defn p20-2 []
  (let [[decoder grid] day20-data]
    (->> (decode-times decoder grid 50)
         (map #(reduce + %))
         (reduce +))))
