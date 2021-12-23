(ns whoops.p23)


(defn add-connection [room start end]
  (update
   room
   start
   #(if %
      (conj % end)
      (set [end]))))

(defn build-hallway []
  (let [positions (range 11)]
    (reduce
     (fn [hallway position]
       (cond-> hallway
         (> position 0) (add-connection ["h" position] ["h" (dec position)])  
         (< position 10) (add-connection ["h" position] ["h" (inc position)])))
     {}
    positions)))

(defn attach-room [room letter attachment]
  (-> room
      (assoc [letter 0] #{[letter 1]})
      (assoc [letter 1] #{[letter 0] ["h" attachment]})
      (update ["h" attachment] #(conj % [letter 1]))))

(defn build-rooms [room]
  (-> room
      (attach-room "A" 2)
      (attach-room "B" 4)
      (attach-room "C" 6)
      (attach-room "D" 8)))

(def part1-room
  (-> (build-hallway)
      build-rooms))

(def move-costs
  {"A" 1
   "B" 10
   "C" 100
   "D" 1000})

;; Advent Room
;; #############
;; #...........#
;; ###A#C#B#A###
;;   #D#D#B#C#
;;   #########

(def part1-advent-start
  {["A" 0] ["A" 1]
   ["D" 0] ["A" 0]
   ["C" 0] ["B" 1]
   ["D" 1] ["B" 0]
   ["B" 0] ["C" 1]
   ["B" 1] ["C" 0]
   ["A" 1] ["D" 1]
   ["C" 1] ["D" 0]})

(def part1-bugs [["A" 0] ["A" 1] ["B" 0] ["B" 1] ["C" 0] ["C" 1] ["D" 0] ["D" 1]])

(defn finished? [bug positions bugs]
  (let [position (positions bug)
        [letter num] bug
        other-bugs (filter #(and
                             (not= bug %)
                             (= letter (first (positions %))))
                           bugs)
        bad-bugs (filter #(not= letter (first %)) other-bugs)
        target-pos (or 0
                       (inc (apply max (map #(second (positions %)) other-bugs))))]
    (and
     (= letter (first position))
     (<= target-pos num)
     (empty? bad-bugs))))

(defn doorway? [position]
  (let [[letter num] position]
    (and (= "h" letter)
         (or (= 2 num)
             (= 4 num)
             (= 6 num)
             (= 8 num)))))

(defn occupying [position positions]
  (first (first (filter #(= (second %) position) positions))))

(defn solved? [positions]
  (every? (fn [[bug position]]
            (= (first bug) (first position))) positions))

(defn combine-states [states new-states]
  (merge-with min states new-states))

(defn reachable-moves
  ([bug positions score room bugs]
   (if (finished? bug positions bugs)
     {}
     (reachable-moves bug (dissoc positions bug) score (positions bug) [(positions bug)] room)))
  ([bug positions score current-position path room]
   (let [path-set (set path)
         available (->> (room current-position)
                        (filter #(not (path-set %)))
                        (filter #(not (occupying % positions))))
         new-score (+ score (move-costs (first bug)))
         states (map (fn [pos]
                       [(assoc positions bug pos) new-score]) available)
         next-states (mapcat #(reachable-moves bug positions new-score % (conj path %) room) available)]
     (concat states next-states))))

(defn hallway? [position]
  (= "h" (first position)))

(defn legal-state? [bug start-pos positions bugs]
  (let [position (positions bug)]
    (cond
      (doorway? position)
      false

      (and (hallway? start-pos)
           (not (finished? bug positions bugs)))
      false
      
      (and (not (hallway? start-pos))
           (not (hallway? position)))
      false

      :else true)))

(defn legal-moves [bug positions score room bugs]
  (let [states (reachable-moves bug positions score room bugs)]
    (into {}
          (filter (fn [[state _score]]
                    (legal-state? bug (positions bug) state bugs))
                  states))))

(defn next-states [[positions score] room bugs]
  (let [already-finished (count (set (map #(finished? % positions bugs) bugs)))
        new-states (->> bugs
                        (mapcat #(legal-moves % positions score room bugs)))
        finishers (filter
                   (fn [[positions _score]]
                     (> (count (set (map #(finished? % positions bugs) bugs)))
                        already-finished))
                   new-states)]
    (if (seq finishers)
      finishers
      new-states)))

(defn organize-room [positions room bugs]
  (loop [best Long/MAX_VALUE
         states {}
         new-states {positions 0}]
    (if (empty? new-states)
      best
      (let [states (combine-states states new-states)
            new-states (combine-states {} (apply concat (pmap #(next-states % room bugs) new-states)))
            winners (filter #(solved? (first %)) new-states)
            winning-scores (map second winners)
            best (apply min (conj winning-scores best))
            new-states (into {} (->> new-states
                                     (filter #(<= (second %) best))
                                     (filter #(or (nil? (states (first %)))
                                                  (<= (second %) (states (first %)))))))]
        (println "Winners:" (count winners))
        (println "New States:" (count new-states))
        (println "States: " (count states))
        (println "Best:" best)
        (println "\n\n")
        (recur best states new-states)))))

(defn p23-1 []
  (organize-room part1-advent-start part1-room part1-bugs))

(defn part2-attach-room [room letter attachment]
  (-> room
      (assoc [letter 0] #{[letter 1]})
      (assoc [letter 1] #{[letter 0] [letter 2]})
      (assoc [letter 2] #{[letter 1] [letter 3]})
      (assoc [letter 3] #{[letter 2] ["h" attachment]})
      (update ["h" attachment] #(conj % [letter 3]))))

(defn part2-build-rooms [room]
  (-> room
      (part2-attach-room "A" 2)
      (part2-attach-room "B" 4)
      (part2-attach-room "C" 6)
      (part2-attach-room "D" 8)))

(def part2-room
  (-> (build-hallway)
      part2-build-rooms))

(def part2-bugs [["A" 0] ["A" 1] ["A" 2] ["A" 3]
                 ["B" 0] ["B" 1] ["B" 2] ["B" 3]
                 ["C" 0] ["C" 1] ["C" 2] ["C" 3]
                 ["D" 0] ["D" 1] ["D" 2] ["D" 3]])

;; Part2 Advent Room
;; #############
;; #...........#
;; ###A#C#B#A###
;;   #D#C#B#A#
;;   #D#B#A#C#
;;   #D#D#B#C#
;;   #########

(def part2-advent-start
  {["A" 0] ["A" 3]
   ["D" 0] ["A" 2]
   ["D" 1] ["A" 1]
   ["D" 2] ["A" 0]
   ["C" 0] ["B" 3]
   ["C" 1] ["B" 2]
   ["B" 0] ["B" 1]
   ["D" 3] ["B" 0]
   ["B" 1] ["C" 3]
   ["B" 2] ["C" 2]
   ["A" 1] ["C" 1]
   ["B" 3] ["C" 0]
   ["A" 2] ["D" 3]
   ["A" 3] ["D" 2]
   ["C" 2] ["D" 1]
   ["C" 3] ["D" 0]})

(defn p23-2 []
  (organize-room part2-advent-start part2-room part2-bugs))
