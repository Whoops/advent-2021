(ns whoops.p7
  (:require [whoops.common :refer [read-input parse-int]]
            [clojure.string :as string]))

(def day7-data (as-> (read-input "7-1") $
                      (first $)
                      (string/split $ #",")
                      (map parse-int $)))

(def max-pos (apply max day7-data))

(defn- distance [a b]
  (Math/abs (- a b)))

(defn- distance-sum [a b]
  (let [d (distance a b)]
    (-> d
        (+ 1)
        (* d)
        (/ 2))))

(defn constant-fuel [target]
  (->> day7-data
       (map #(distance target %))
       (reduce +)))

(defn incremental-fuel [target]
  (->> day7-data
       (map #(distance-sum target %))
       (reduce +)))

(defn min-fuel [consume-fn]
  (reduce (fn [mn nxt]
            (min mn (consume-fn nxt))) Long/MAX_VALUE (range (inc max-pos))))

(defn p7-1 []
  (min-fuel constant-fuel))

(defn p7-2 []
  (min-fuel incremental-fuel))
