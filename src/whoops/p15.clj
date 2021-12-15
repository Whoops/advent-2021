(ns whoops.p15
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [whoops.common :refer [parse-long read-input]]))

(defn parse-input [input]
  (->> input
       (mapv vec)
       (mapv #(mapv str %))
       (mapv #(mapv parse-long %))))

(def day15-data (parse-input (read-input "15-1")))


(def sample-data (-> "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"
                     string/split-lines
                     parse-input))


(defn dims [grid]
  [(count (first grid)) (count grid)])

(defn out-of-bounds? [[x y] grid]
  (let [[max-x max-y] (dims grid)]
    (or (neg? x)
        (neg? y)
        (> x (dec max-x))
        (> y (dec max-y)))))
(defn surrounding-points [[x y] grid]
  (let [pts [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]]
    (remove #(out-of-bounds? % grid) pts)))

(defn get-point [[x y] grid]
  (-> grid
      (nth y)
      (nth x)))

(defn search-point [point paths grid]
  (let [[path score] (paths point)
        surrounding (surrounding-points point grid)]
    (reduce (fn [[pts paths] point]
              (let [new-score (+ score (get-point point grid))
                    new-path (conj path point)
                    [_ existing-score] (paths point)]
                (if (or (not existing-score) (> existing-score new-score))
                  [(conj pts point) (assoc paths point [new-path new-score])]
                  [pts paths])))
            [#{} paths]
            surrounding)))

(defn search-step [points paths grid]
  (reduce (fn [[pts paths] point]
            (let [[new-pts new-paths] (search-point point paths grid)]
              [(set/union new-pts pts) new-paths]))
          [#{} paths]
          points))

(defn explore-grid [grid]
  (loop [pts [[0 0]]
         paths {[0 0] [[[0 0]] 0]}]
    (let [[new-points new-paths] (search-step pts paths grid)]
      (if (seq new-points)
        (recur new-points new-paths)
        paths))))

(defn inc-row [row]
  (->> row
       (map inc)
       (map #(if (> % 9) (mod % 9) %))))

(defn expand-grid [grid factor]
  (let [rows (for [row grid]
               (->> row
                    (iterate inc-row)
                    (take factor)
                    (apply concat)
                    vec))]
    (->> rows
         (iterate #(mapv inc-row %))
         (take factor)
         (apply concat)
         vec)))

(defn p15-1 []
  (let [mapping (explore-grid day15-data)
        [width height] (dims day15-data)]
    (second (mapping [(dec width) (dec height)]))))

(defn p15-2 []
  (let [grid (expand-grid day15-data 5)
        [width height] (dims grid)
        mapping (explore-grid grid)]
    (second (mapping [(dec width) (dec height)]))))
