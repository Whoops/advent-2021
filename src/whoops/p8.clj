(ns whoops.p8
  (:require [whoops.common :refer [read-input parse-long]]
            [clojure.string :as string]
            [clojure.set :as set]))

(def day8-data (->> (read-input "8-1")
                    (map #(string/split %  #" "))
                    (map #(map set %))
                    (map (fn [lst] (partition-by #(= #{\|} %) lst)))
                    (map (juxt first last))))

(defn find-zero [three six data]
  (->> data
       (filter #(= 6 (count %)))
       (filter #(not= six %))
       (filter #(= 1 (count (set/difference three %))))
       first))

(defn find-one [data]
  (->> data
       (filter #(= 2 (count %)))
       first))

(defn find-two [three five data]
  (->> data
       (filter #(= 5 (count %)))
       (filter #(= 1 (count (set/difference % three))))
       (filter #(not= five %))
       first))

(defn find-three [seven data]
  (->> data
       (filter #(= 5 (count %)))
       (filter #(= 3 (count (set/intersection seven %))))
       first))

(defn find-four [data]
  (->> data
       (filter #(= 4 (count %)))
       first))

(defn find-five [six data]
  (->> data
       (filter #(= 5 (count %)))
       (filter #(= 1 (count (set/difference six %))))
       first))

(defn find-six [one data]
  (->> data
       (filter #(= 6 (count %)))
       (filter #(= 1 (count (set/intersection one %))))
       first))

(defn find-seven [data]
  (->> data
       (filter #(= 3 (count %)))
       first))

(defn find-eight [data]
  (->> data
       (filter #(= 7 (count %)))
       first))

(defn find-nine [three six data]
  (->> data
       (filter #(= 6 (count %)))
       (filter #(not= six %))
       (filter #(= 0 (count (set/difference three %))))
       first))

(defn decode [[samples display]]
  (let [one (find-one samples)
        four (find-four samples)
        seven (find-seven samples)
        eight (find-eight samples)
        three (find-three seven samples)
        six (find-six one samples)
        zero (find-zero three six samples)
        five (find-five six samples)
        two (find-two three five samples)
        nine (find-nine three six samples)
        mapping {zero 0
                 one 1
                 two 2
                 three 3
                 four 4
                 five 5
                 six 6
                 seven 7
                 eight 8
                 nine 9}]
    (->> display
         (map mapping)
         string/join
         parse-long)))

(defn p8-1 []
  (->> day8-data
       (map second)
       flatten
       (filter #(#{2 3 7 4} (count %)))
       count))

(defn p8-2 []
  (->> day8-data
       (map decode)
       (reduce +)))
