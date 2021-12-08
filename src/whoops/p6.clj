(ns whoops.p6
  (:require
   [clojure.string :as string]
   [whoops.common :refer [parse-int read-input]]))

(def day6-data (->> (read-input "6-1")
                    first
                    (#(string/split % #","))
                    (map parse-int)
                    frequencies))

(defn run-step [freqs]
  [(get freqs 1 0)
   (get freqs 2 0)
   (get freqs 3 0)
   (get freqs 4 0)
   (get freqs 5 0)
   (get freqs 6 0)
   (+ (get freqs 0 0) (get freqs 7 0))
   (get freqs 8 0)
   (get freqs 0 0)])


(defn parse-string [s]
  (frequencies (map parse-int (string/split s #","))))

(defn p6-1 []
  (as-> day6-data $
      (iterate run-step $)
      (nth $ 80)
      (reduce + $)))

(defn p6-2 []
  (as-> day6-data $
      (iterate run-step $)
      (nth $ 256)
      (reduce + $)))
