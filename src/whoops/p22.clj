(ns whoops.p22
  (:require
   [clojure.string :as string]
   [whoops.common :refer [parse-long read-input]]))

(defn parse-line [[flip & coords]]
  (let [coords (->> coords
                    (map #(re-seq #"-?\d+" %))
                    (map #(mapv parse-long %)))]
    [(keyword flip) coords]))

(defn normalize-line [[flip cube]]
  [flip (map sort cube)])

(defn parse-input [input]
  (->> input
       (map #(string/split % #"\s|,"))
       (map parse-line)
       (map normalize-line)))
(def day22-data
  (parse-input (read-input "22-1")))

(defn range-overlap? [[r1-min r1-max] [r2-min r2-max]]
  (or (>= r2-max r1-min r2-min)
      (>= r2-max r1-max r2-min)
      (>= r1-max r2-min r1-min)
      (>= r1-max r2-max r1-min)))

(defn overlap? [cube1 cube2]
  (every? identity (map range-overlap? cube1 cube2)))
(defn overlap [cube1 cube2]
  (when (overlap? cube1 cube2)
    (let [[[x1-min x1-max] [y1-min y1-max] [z1-min z1-max]] cube1
          [[x2-min x2-max] [y2-min y2-max] [z2-min z2-max]] cube2]
      [[(max x1-min x2-min) (min x1-max x2-max)]
       [(max y1-min y2-min) (min y1-max y2-max)]
       [(max z1-min z2-min) (min z1-max z2-max)]])))

(defn volume [cube]
  (->> cube
       (map #(reduce - %))
       (map #(Math/abs %))
       (map inc)
       (reduce *)))

(defn valid-cube? [[[x-min x-max] [y-min y-max] [z-min z-max]]]
  (and (<= x-min x-max)
       (<= y-min y-max)
       (<= z-min z-max)))

(defn split-cube [remove cube]
  (let [[[x-remove-min x-remove-max] [y-remove-min y-remove-max] [z-remove-min z-remove-max]] remove
        [[x-min x-max] [y-min y-max] [z-min z-max]] cube]
    (filter valid-cube?
            (rest
             (for [x [[x-remove-min x-remove-max] [x-min (dec x-remove-min)] [(inc x-remove-max) x-max]]
                   y [[y-remove-min y-remove-max] [y-min (dec y-remove-min)] [(inc y-remove-max) y-max]]
                   z [[z-remove-min z-remove-max] [z-min (dec z-remove-min)] [(inc z-remove-max) z-max]]]
               [x y z])))))

(defn apply-rule [existing-rule new-rules]
  (let [[_flip existing-cube] existing-rule]
    (reduce (fn [new-rules new-rule]
              (let [[new-flip new-cube] new-rule]
                (if-let [to-remove (overlap new-cube existing-cube)]
                  (concat new-rules (map (fn [cube] [new-flip cube]) (split-cube to-remove new-cube)))
                  (conj new-rules new-rule))))
            []
            new-rules)))

(defn apply-rules [existing-rules new-rule]
  (loop [rules existing-rules
         new-rules [new-rule]]
    (if-let [rule (first rules)]
      (recur (rest rules) (apply-rule rule new-rules))
      (concat existing-rules new-rules))))

(defn in-init-bounds? [[_flip cube]]
  (->> cube
       flatten
       (map #(Math/abs %))
       (every? #(<= % 50))))

(defn p22-1 []
  (->> day22-data
       (filter in-init-bounds?)
       reverse
       (reduce apply-rules [])
       (remove #(= :off (first %)))
       (map #(volume (second %)))
       (reduce +)))

(defn p22-2 []
  (->> day22-data
       reverse
       (reduce apply-rules [])
       (remove #(= :off (first %)))
       (map #(volume (second %)))
       (reduce +)))
