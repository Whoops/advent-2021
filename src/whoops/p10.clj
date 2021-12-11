(ns whoops.p10
  (:require [whoops.common :refer [read-input]]))

(def day10-data (read-input "10-1"))

(def delims {\( \)
             \[ \]
             \{ \}
             \< \>})

(def opening? (set (keys delims)))
(def closing? (set (vals delims)))
(def error-score {\) 3
                  \] 57
                  \} 1197
                  \> 25137})

(def complete-score {\) 1
                     \] 2
                     \} 3
                     \> 4})

(defn error-score-line [line]
  (loop [opens '()
         line line]
    (let [c  (first line)
          line (rest line)]
      (cond
        (nil? c) 0
        (opening? c) (recur (conj opens c) line)
        (= c (delims (peek opens))) (recur (pop opens) line)
        :else (error-score c)))))

(defn unclosed-line [line]
  (loop [opens '()
         line line]
    (let [c  (first line)
          line (rest line)]
      (cond
        (nil? c) opens
        (opening? c) (recur (conj opens c) line)
        (= c (delims (peek opens))) (recur (pop opens) line)))))

(defn complete-line [line]
  (let [unclosed (unclosed-line line)]
    (map delims unclosed)))

(defn- accumulate-score [score c]
  (-> score
      (* 5)
      (+ (complete-score c))))

(defn score-competion [completion]
  (reduce accumulate-score 0 completion))

(defn middle [lst]
  (let [midpoint (Math/floor (/ (count lst) 2))]
    (nth lst midpoint)))

(defn p10-1 []
  (->> day10-data
       (map error-score-line)
       (reduce +)))

(defn p10-2 []
  (->> day10-data
       (filter #(= 0 (error-score-line %)))
       (map complete-line)
       (map score-competion)
       sort
       middle))
