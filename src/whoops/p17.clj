(ns whoops.p17)

(defn- distance-sum [d]
  (-> d
      (+ 1)
      (* d)
      (/ 2)))


(defn new-x-velocity [v]
  (cond
    (> v 0) (dec v)
    (< v 0) (inc v)
    :else 0))

(defn run-step [[[x y] [x-vel y-vel]]]
   [[(+ x x-vel) (+ y y-vel)] [(new-x-velocity x-vel) (dec y-vel)]])

(defn x-works? [x-vel x-min x-max]
  (->> (iterate run-step [[0 0] [x-vel 0]])
       (take-while #(> (first (second %)) 0))
       (map #(first (first %)))
       (filter #(<= x-min % x-max))
       (first)))


(defn right-direction? [pos vel target]
  (or (pos? vel) (zero? vel) (and (>= pos target) (neg? vel))))

(defn y-works? [y-vel y-min y-max]
  (->> (iterate run-step [[0 0] [0 y-vel]])
       (take-while #(right-direction? (second (first %)) (second (second %)) y-min))
       (map #(second (first %)))
       (filter #(<= y-min % y-max))
       (first)))

(defn search-x [min-x max-x]
  (filter #(x-works? % min-x max-x) (range (inc max-x))))

(defn search-y [min-y max-y]
  (filter #(y-works? % min-y max-y) (range (- 0 (Math/abs min-y)) (inc (Math/abs min-y)))))

(defn on-target? [[x y] x-min x-max y-min y-max]
  (and (<= x-min x x-max)
       (<= y-min y y-max)))

(defn works? [x-vel y-vel x-min x-max y-min y-max]
  (->> (iterate run-step [[0 0] [x-vel y-vel]])
       (take-while #(right-direction? (second (first %)) (second (second %)) y-min))
       (filter #(on-target? (first %) x-min x-max y-min y-max))
       first))

;; target area: x=175..227, y=-134..-79

(defn p17-1 []
  (distance-sum (apply max (search-y -134 -79))))

(defn p17-2 []
  (let [x-min 175
        x-max 227
        y-min -134
        y-max -79
        x-vels (search-x x-min x-max)
        y-vels (search-y y-min y-max)]
    (count (for [x-vel x-vels
                 y-vel y-vels
                 :when (works? x-vel y-vel x-min x-max y-min y-max)]
             [x-vel y-vel]))))
