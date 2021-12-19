(ns whoops.p19
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [whoops.common :refer [parse-long read-input]]))

(defn parse-input [input]
  (loop [parsed []
         [current todo] (split-with #(not= "" %) input)]
    (let [scanner (parse-long (re-find #"\d+" (first current)))
          beacons (->> (rest current) (map #(str "[" % "]")) (map edn/read-string) vec)
          parsed (conj parsed {:name scanner :beacons beacons :aligned false})]
      (if (seq todo)
        (recur parsed (split-with #(not= "" %) (drop 1 todo)))
        parsed))))

(def day19-data (parse-input (read-input "19-1")))

(defn square [x]
  (* x x))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (Math/sqrt (+ (square (- x1 x2)) (square (- y1 y2)) (square (- z1 z2)))))

(defn beacon-distances [beacons]
  (into {}
        (for [beacon beacons]
          [beacon (sort (map #(distance beacon %) (remove #(= beacon %) beacons)))])))

(defn same-beacon? [distances1 distances2]
  (>= (count (set/intersection (set distances1) (set distances2))) 11))

(defn align-beacons [beacons1 beacons2]
  (let [distances1 (beacon-distances beacons1)
        distances2 (beacon-distances beacons2)]
    (for [[b1 distances1] distances1
          [b2 distances2] distances2
          :when (same-beacon? distances1 distances2)]
      [b1 b2])))

(defn overlaps? [scanner1 scanner2]
  (>= (count (align-beacons (:beacons scanner1) (:beacons scanner2))) 12))

(defn align-first [scanners]
  (-> scanners
      (assoc-in [0 :aligned] true)
      (assoc-in [0 :position] [0 0 0])))

(defn find-overlaps [scanners]
  (let [pairs (for [s1 scanners
                    s2 scanners
                    :when (and (< (:name s1) (:name s2))
                               (overlaps? s1 s2))]
                [(:name s1) (:name s2)])]
    (reduce (fn [scanners [s1-name s2-name]]
              (-> scanners
                  (update-in [s1-name :overlaps] conj s2-name)
                  (update-in [s2-name :overlaps] conj s1-name)))
            scanners
            pairs)))

(defn aligned? [scanners]
  (every? :aligned scanners))

(def orientations (for [x-mod [-1 1]
                        y-mod [-1 1]
                        z-mod [-1 1]
                        x [0 1 2]
                        y [0 1 2]
                        z [0 1 2]
                        :when (distinct? x y z)]
                    [[x-mod x] [y-mod y] [z-mod z]]))

(defn transform-point [point orientation]
  (mapv (fn [[mod idx]] (* mod (nth point idx))) orientation))

(defn shift-point [point new-origin]
  (mapv #(+ %1 %2) point new-origin))

(defn realign-scanner [scanner position orientation]
  (let [beacons (map #(shift-point (transform-point % orientation) position) (:beacons scanner))]
    (assoc scanner
           :aligned true
           :position position
           :beacons beacons)))

(defn align-scanner [reference unaligned]
  (let [beacon-pairs (align-beacons (:beacons reference) (:beacons unaligned))]
    (loop [orientations orientations]
      (let [orientation (first orientations)
            [reference-beacon align-beacon] (first beacon-pairs)
            [reference2 test-beacon] (second beacon-pairs)
            reoiriented (transform-point align-beacon orientation)
            scanner-pos (mapv #(- %1 %2) reference-beacon reoiriented)
            test-reoriented (transform-point test-beacon orientation)
            test-shifted (shift-point test-reoriented scanner-pos)]
        (if (= reference2 test-shifted)
          (realign-scanner unaligned scanner-pos orientation)
          (recur (rest orientations)))))))

(defn find-reference [scanner scanners]
  (some (fn [idx]
          (when (get-in scanners [idx :aligned])
            (nth scanners idx)))
        (:overlaps scanner)))

(defn align-scanners [scanners]
  (let [scanners (find-overlaps (align-first scanners))]
    (loop [scanners scanners]
      (if (aligned? scanners)
        scanners
        (let [unaligned (remove :aligned scanners)
              target (first (filter #(find-reference % scanners) unaligned))
              reference (find-reference target scanners)]
          (recur (assoc scanners (:name target) (align-scanner reference target))))))))

(defn manhatten-distance [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)) (Math/abs (- z1 z2))))

(defn p19-1 []
  (->> day19-data
       align-scanners
       (map :beacons)
       (apply concat)
       set count))
(defn p19-2 []
  (let [aligned (align-scanners day19-data)]
    (apply max
           (for [s1 aligned
                 s2 aligned]
             (manhatten-distance (:position s1) (:position s2))))))
