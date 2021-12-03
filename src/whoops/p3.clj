(ns whoops.p3
  (:require
   [clojure.string :as string]
   [whoops.common :refer [parse-int read-input]]))


(defn- process-row [s]
  (->> s
      vec
      (map (comp parse-int str))))

(def ^:private p3-data
  (map process-row (read-input "3-1")))

(defn- sum-data [data]
  (->> data
      (apply map vector)
      (map #(reduce + %))))

(defn- gamma-vec [data]
  (let [sums (sum-data data)
        threshold (/ (count data) 2)]
    (map #(if (>= % threshold) 1 0) sums)))

(defn- epsilon-vec [data]
  (let [sums (sum-data data)
        threshold (/ (count data) 2)]
    (map #(if (> threshold %) 1 0) sums)))

(defn- vec->int [v]
  (-> v
      string/join
      (parse-int 2)))

(defn- rating [data idx rating-fn]
  (if (= 1 (count data))
     (first data)
     (let [target (nth (rating-fn data) idx)
           matching (filter #(= target (nth % idx)) data)]
       (rating matching (inc idx) rating-fn))))

(defn- oxygen-rating [data]
  (vec->int (rating data 0 gamma-vec)))

(defn- co2-rating [data]
  (vec->int (rating data 0 epsilon-vec)))
(defn p3-1 []
  (let [gamma (vec->int (gamma-vec p3-data))
        epsilon (vec->int (epsilon-vec p3-data))]
    (* gamma epsilon)))

(defn p3-2 []
  (let [oxy (oxygen-rating p3-data)
        co2 (co2-rating p3-data)]
    (* oxy co2)))
