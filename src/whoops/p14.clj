(ns whoops.p14
  (:require
   [clojure.string :as string]
   [whoops.common :refer [read-input]]))

(defn parse-rule [rule]
  (let [[pair sub] (map vec (string/split rule #" -> "))]
    {pair (first sub)}))

(defn parse-input [input]
  (let [[start _ rules] (partition-by #(= "" %) input)
        start (first start)
        rules (into {} (map parse-rule rules))]
    [start rules]))

(defn pairs [line]
  (frequencies (map vec (partition-all 2 1 line))))

(defn update-pair [pairs pair additional]
  (let [current-count (get pairs pair 0)]
    (assoc pairs pair (+ current-count additional))))

(defn do-insertions [pairs insertions]
  (reduce (fn [new-pairs [[fst snd] cnt]]
            (let [pair [fst snd]
                  insertion (insertions pair)
                  new-pair1 [fst insertion]
                  new-pair2 [insertion snd]]
              (if snd
                (-> new-pairs
                    (update-pair new-pair1 cnt)
                    (update-pair new-pair2 cnt))
                (assoc new-pairs [fst] 1))))
          {}
          pairs))

(defn count-occurances [pairs]
  (reduce (fn [counts [[fst _snd] cnt]]
            (let [current-count (get counts fst 0)]
              (assoc counts fst (+ cnt current-count))))
          {}
          pairs))
(def day14-data (parse-input (read-input "14-1")))

(defn p14-1 []
  (let [[start rules] day14-data
        freqs (->> start
                   pairs
                   (iterate #(do-insertions % rules))
                   (take 11)
                   last
                   count-occurances
                   vals
                   sort)]
    (- (last freqs) (first freqs))))

(defn p14-2 []
  (let [[start rules] day14-data
        freqs (->> start
                   pairs
                   (iterate #(do-insertions % rules))
                   (take 41)
                   last
                   count-occurances
                   vals
                   sort)]
    (- (last freqs) (first freqs))))
