(ns whoops.p4
  (:require [whoops.common :refer [read-input parse-int]]
            [clojure.string :as string]))


(def ^:private day4-input (read-input "4-1"))

(def ^:private calls (as-> day4-input $
             (first $)
             (string/split $ #",")
             (map parse-int $)))

(defn- split-board [raw-board]
  (->> raw-board
       (map #(string/split % #" +"))
       (map #(remove string/blank? %))
       (map #(map parse-int %))))

(def ^:private boards (->> day4-input
                rest
                (partition-by #(= "" %))
                (filter #(not= '("") %))
                (map split-board)))

(defn- cols [board]
  (apply map vector board))

(defn- board-wins? [board calls]
  (->> board
       (concat (cols board))
       (map #(filter (set calls) %))
       (filter #(= (count %) (count board)))
       seq
       boolean))

(defn- board-score [board calls]
  (when (board-wins? board calls)
    (->> board
         flatten
         (remove (set calls))
         (reduce +)
         (* (last calls)))))

(defn- rank-board [board calls]
  (loop [cnt (count board)]
    (if (board-wins? board (take cnt calls))
      [cnt (board-score board (take cnt calls)) board]
      (recur (inc cnt)))))

(def ^:private board-ranking
  (->> boards
       (map #(rank-board % calls))
       (sort-by first)))

(defn p4-1 []
  (let [[_ score _] (first board-ranking)]
    score))

(defn p4-2 []
  (let [[_ score _] (last board-ranking)]
    score))
