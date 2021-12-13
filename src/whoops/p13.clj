(ns whoops.p13
  (:require
   [clojure.string :as string]
   [whoops.common :refer [parse-long read-input]]))

(defn parse-cord [line]
  (mapv parse-long (string/split line #",")))

(defn parse-fold [line]
  (let [axis (re-find #"[x|y]" line)
        digit (parse-long (re-find #"\d+" line))]
    [axis digit]))

(defn parse-input [input]
  (let [[cords _ folds] (partition-by #(= "" %) input)
        cords (map parse-cord cords)
        folds (map parse-fold folds)]
    [cords folds]))

(defn fold-cord [[x y] [axis line]]
  (cond
    (and (= "x" axis) (> x line)) [(- x (* 2 (- x line))) y]
    (and (= "y" axis) (> y line)) [x (- y (* 2 (- y line)))]
    :else [x y]))

(def day13-data (parse-input (read-input "13-1")))


(defn grid-dims [cords]
  [(inc (apply max (map first cords))) (inc (apply max (map second cords)))])

(defn mark-point [grid [x y]]
  (assoc-in grid [y x] true))
(defn draw-grid [cords]
  (let [[width height] (grid-dims cords)
        grid (make-array Boolean/TYPE height width)
        grid (mapv vec grid)
        grid (reduce mark-point grid cords)]
    (doseq [row grid]
      (doseq [it row]
        (if it
          (print "#")
          (print ".")))
      (println))))

(defn do-fold [cords fold]
  (map #(fold-cord % fold) cords))

(defn p13-1 []
  (let [[cords folds] day13-data]
    (-> cords
        (do-fold (first folds))
        set
        count)))


(defn p13-2 []
  (let [[cords folds] day13-data]
    (->> folds
         (reduce do-fold cords)
         draw-grid)))
