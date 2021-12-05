(ns whoops.p5
  (:require [whoops.common :refer [read-input parse-int]]))

(def ^:private day5-input (read-input "5-1"))

(defn- parse-vector [s]
  (->> s
      (re-seq #"\d+")
      (map parse-int)
      (partition 2)))

(defn- flat? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn- expand-vector [[[x1 y1] [x2 y2]]]
  (let [x-steps (- x2 x1)
        y-steps (- y2 y1)
        steps (max (Math/abs x-steps) (Math/abs y-steps))
        x-step (/ x-steps steps)
        y-step (/ y-steps steps)]
    (loop [v [[x1 y1]]
           step 1]
      (if (> step steps)
        v
        (recur (conj v [(+ x1 (* x-step step)) (+ y1 (* y-step step))])
               (inc step))))))

(defn p5-1 []
  (->> day5-input
       (map parse-vector)
       (filter flat?)
       (map expand-vector)
       (apply concat)
       frequencies
       (filter #(>= (second %) 2))
       count))

(defn p5-2 []
  (->> day5-input
       (map parse-vector)
       (map expand-vector)
       (apply concat)
       frequencies
       (filter #(>= (second %) 2))
       count))
