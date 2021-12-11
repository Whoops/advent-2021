(ns whoops.p9
  (:require [whoops.common :refer [read-input parse-long]]
            [clojure.string :as string]))

(def day9-data
  (->> (read-input "9-1")
       (mapv vec)
       (mapv #(mapv str %))
       (mapv #(mapv parse-long %))))


(defn get-point [[x y] grid]
  (let [xMax (count (first grid))
        yMax (count grid)]
    (if (or (< x 0) (>= x xMax) (< y 0) (>= y yMax))
      9
      (nth (nth grid y) x))))

(defn low-point? [[x y] grid]
  (let [height (get-point [x y] grid)]
    (and (< height (get-point [(inc x) y] grid))
         (< height (get-point [(dec x) y] grid))
         (< height (get-point  [x (inc y)] grid))
         (< height (get-point [x (dec y)] grid)))))

(defn low-points [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (low-point? [x y] grid)]
    [x y]))

(defn risk [point grid]
  (inc (get-point point grid)))

(defn surrounding-points [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(defn map-basin
  ([point grid] (map-basin point grid #{}))
  ([point grid basin]
   (if (or (basin point) (= 9 (get-point point grid)))
     basin
     (let [basin (conj basin point)
           surrounding (surrounding-points point)]
       (reduce #(map-basin %2 grid %1) basin surrounding)))))

(defn p9-1 []
  (->> day9-data
      low-points 
      (map #(risk % day9-data))
      (reduce +)))


(defn p9-2 []
  (->> day9-data
       low-points
       (map #(map-basin % day9-data))
       (map count)
       sort
       reverse
       (take 3)
       (reduce *)))
