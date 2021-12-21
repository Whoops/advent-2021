(ns whoops.p21)


(def deterministic-die (cycle (range 1 101)))

(defn advance-player [current roll]
  (-> roll
      (+ current)
      dec
      (mod 10)
      inc))

(defn run-game [p1-start p2-start]
  (loop [p1-loc p1-start
         p2-loc p2-start
         p1-score 0
         p2-score 0
         rolls 0
         die deterministic-die]
    (let [[r1 r2 r3 & die] die
          [r4 r5 r6 & die] die
          p1-loc (advance-player p1-loc (+ r1 r2 r3))
          p2-loc (advance-player p2-loc (+ r4 r5 r6))
          p1-new-score (+ p1-score p1-loc)
          p2-new-score (+ p2-score p2-loc)]
      (cond
        (>= p1-new-score 1000) [p1-new-score p2-score (+ rolls 3)]
        (>= p2-new-score 1000) [p1-new-score p2-new-score (+ rolls 6)]
        :else (recur p1-loc p2-loc p1-new-score p2-new-score (+ rolls 6) die)))))

(def quantum-die
  (seq
   (frequencies
        (for [x (range 1 4)
              y (range 1 4)
              z (range 1 4)]
          (+ x y z)))))

(defn initial-state [p1-start p2-start]
  {:scores [0 0]
   :positions [p1-start p2-start]})

(defn new-states [state cnt player]
  (reduce (fn [new-states [roll num]]
            (let [new-pos (advance-player (get-in state [:positions player]) roll)
                  num num
                  new-state (-> state
                                (assoc-in [:positions player] new-pos)
                                (update-in [:scores player] #(+ new-pos %)))]
              (conj new-states [new-state (* cnt num)])))
          []
          quantum-die))

(defn reduce-state [states state cnt player]
  (let [moves (new-states state cnt player)]
    (reduce (fn [states [new-state cnt]]
              (update states new-state #(if % (+ cnt %) cnt)))
            states
            moves)))

(defn has-winner? [[state _cnt]]
  (let [[p1-score p2-score] (:scores state)]
    (or (>= p1-score 21) (>= p2-score 21))))

(defn run-quantum-game [p1-start p2-start]
  (loop [current-player 0
         wins [0 0]
         states {(initial-state p1-start p2-start) 1}]
    (let [new-states (reduce #(reduce-state %1 (first %2) (second %2) current-player) {} states)
          {winning-states true new-states false} (group-by has-winner? new-states)
          win-cnt (->> winning-states
                       (map second)
                       (reduce +))
          new-wins (update wins current-player #(+ win-cnt %))
          new-player (-> current-player inc (mod 2))]
      (if (seq new-states)
        (recur new-player new-wins new-states)
        new-wins))))

(defn p21-1 []
  (let [[p1-score p2-score rolls] (run-game 2 5)]
    (* (min p1-score p2-score) rolls)))

(defn p21-2 []
  (apply max (run-quantum-game 2 5)))
