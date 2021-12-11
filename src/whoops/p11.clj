(ns whoops.p11
  (:require
   [clojure.set :as set]
   [whoops.common :refer [parse-long read-input]]))

(def day11-data
  (->> (read-input "11-1")
       (mapv vec)
       (mapv #(mapv str %))
       (mapv #(mapv parse-long %))))

(defn dimensions [grid]
  [(count (first grid)) (count grid)])
(defn get-point [grid [x y]]
  (let [[x-max y-max] (dimensions grid)]
    (if (or (< x 0) (>= x x-max) (< y 0) (>= y y-max))
      0
      (nth (nth grid y) x))))

(defn inc-point [grid [x y]]
  (let [[x-max y-max] (dimensions grid)]
    (if (or (< x 0) (>= x x-max) (< y 0) (>= y y-max))
      grid
      (update-in grid [y x] #(min (inc %) 10)))))

(defn increment-grid [grid]
  (mapv #(mapv inc %) grid))

(defn find-ready-to-flash [grid]
  (set (for [x (range (count (first grid)))
             y (range (count grid))
             :when (= 10 (get-point grid [x y]))]
         [x y])))

(defn surrounding-points [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]
    [(inc x) (inc y)] [(inc x) (dec y)] [(dec x) (inc y)] [(dec x) (dec y)]})

(defn- flash-point [grid point]
  (let [surrounding (surrounding-points point)
        grid (reduce #(inc-point %1 %2) grid surrounding)
        new-flashers (set (filter #(= 10 (get-point grid %)) surrounding))]
    [grid new-flashers]))

(defn- reduce-flashers [[grid new-flashers] flasher]
  (let [[grid flashers'] (flash-point grid flasher)]
    [grid (set/union new-flashers flashers')]))

(defn propogate-flashers [grid flashers]
  (loop [new-flashers flashers
         flashed flashers
         grid grid]
    (if (not (seq new-flashers))
      grid
      (let [[grid next-flashers] (reduce reduce-flashers [grid #{}] new-flashers)]
        (recur (set/difference next-flashers flashed)
               (set/union new-flashers flashed next-flashers)
               grid)))))

(defn zero-flashed-row [row]
  (mapv #(if (= 10 %) 0 %) row))

(defn zero-flashed [grid]
  (mapv zero-flashed-row grid))

(defn do-step [grid]
  (let [grid (increment-grid grid)
        flashers (find-ready-to-flash grid)
        grid (propogate-flashers grid flashers)]
    (zero-flashed grid)))

(defn- count-flashes [grid]
  (->> grid
       (map #(filter zero? %))
       (map count)
       (reduce +)))

(defn- synced-row? [row]
  (every? #(= 0 %) row))
(defn- synced? [grid]
  (every? synced-row? grid))

(defn p11-1 []
  (->> day11-data
       (iterate do-step)
       (map count-flashes)
       (take 101)
       (reduce +)))

(defn p11-2 []
  (->> day11-data
       (iterate do-step)
       (take-while #(not (synced? %)))
       count))
