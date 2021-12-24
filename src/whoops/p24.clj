(ns whoops.p24
  (:require [whoops.common :refer [read-input parse-long]]
            [clojure.string :as string]))

(defn parse-cmd [[cmd & args]]
  [(keyword cmd) (mapv #(if (re-matches #"-?\d+" %)
                          (parse-long %)
                          (keyword %))
                       args)])

(defn parse-input [input]
  (->> input
       (map #(string/split % #"\s"))
       (map parse-cmd)))

(def day24-prog (parse-input (read-input "24-1")))

(def init-memory {:w 0
                  :x 0
                  :y 0
                  :z 0})

(defmulti eval-cmd
  (fn [[cmd & _] _ _]
    cmd))

(defmethod eval-cmd :inp [[_ [reg]] memory [v & rst]]
  [(assoc memory reg v) rst])

(defmethod eval-cmd :add [[_ [reg reg-val]] memory input]
  (let [val (if (keyword? reg-val)
              (reg-val memory)
              reg-val)]
    [(update memory reg + val) input]))

(defmethod eval-cmd :mul [[_ [reg reg-val]] memory input]
  (let [val (if (keyword? reg-val)
              (reg-val memory)
              reg-val)]
    [(update memory reg * val) input]))

(defmethod eval-cmd :div [[_ [reg reg-val]] memory input]
  (let [val (if (keyword? reg-val)
              (reg-val memory)
              reg-val)]
    [(update memory reg #(long (/ % val))) input]))

(defmethod eval-cmd :mod [[_ [reg reg-val]] memory input]
  (let [val (if (keyword? reg-val)
              (reg-val memory)
              reg-val)]
    [(update memory reg #(mod % val)) input]))

(defmethod eval-cmd :eql [[_ [reg reg-val]] memory input]
  (let [val (if (keyword? reg-val)
              (reg-val memory)
              reg-val)]
    [(update memory reg #(if (= % val) 1 0)) input]))

(defn run-program [cmds input]
  (loop [cmds cmds
         memory init-memory
         input input]
    (if (seq cmds)
      (let [[cmd & cmds] cmds
            [memory input] (eval-cmd cmd memory input)]
        (println cmd ";; " memory "--" (string/join input))
        (recur cmds memory input))
      memory)))

;; Happy path trace of Z
;; z = (((d1 + 12) * 26)
;; z = (26 * d1) + (26 * 12) + d2 + 6
;; z = (26 ^ 2 * d1) + (26^2 * 12) + (26 * d2) + (26 * 6) + d3 + 4
;; z = (26^3 * d1) + (26^3 * 12) + (26^2 * d2) + (26^2 * 6) + (26 * d3) + (26 * 4) + d4 + 5
;; z = (26^4 * d1) + (26^4 * 12) + (26^3 * d2) + (26^3 * 6) + (26^2 * d3) + (26^2 * 4) + (26 * d4) + (26 * 5) + d5
;; z = (26^3 * d1) + (26^3 * 12) + (26^2 * d2) + (26^2 * 6) + (26^1 * d3) + (26^1 * 4) + d4 + 5
;; z = (26^2 * d1) + (26^2 * 12) + (26^1 * d2) + (26^1 * 6) + d3 + 4
;; z = (26^3 * d1) + (26^3 * 12) + (26^2 * d2) + (26^2 * 6) + (26^1 * d3) + (26^1 * 4) + d8 + 14
;; z = (26^2 * d1) + (26^2 * 12) + (26^1 * d2) + (26^1 * 6) + d3 + 4
;; z = (26^3 * d1) + (26^3 * 12) + (26^2 * d2) + (26^2 * 6) + (26^1 * d3) + (26^1 * 4) + d10 + 14
;; z = (26^2 * d1) + (26^2 * 12) + (26^1 * d2) + (26^1 * 6) + d3 + 4 // d11
;; z = (26^1 * d1) + (26^1 * 12) + d2 + 6 // d12
;; z = d1 + 12 // d13

;; contraints to ensure z shrinks when neccesary
;; d6 = d5 - 7
;; d7 = d4 - 8
;; d9 = d8 + 7
;; d11 = d10 + 5
;; d12 = d3 + 2
;; d13 = d2 - 3
;; d14 = d1 - 2

(def legal-digit (range 1 10))

;; getting the min/max from the contraints above would be easy enough,
;; but my brain is getting fuzzy. Here's all the legal numbers, sorted.
(defn legal-numbers []
  (for [d1 legal-digit
        d2 legal-digit
        d3 legal-digit
        d4 legal-digit
        d5 legal-digit
        d8 legal-digit
        d10 legal-digit
        :let [d6 (- d5 7)
              d7 (- d4 8)
              d9 (+ d8 7)
              d11 (+ d10 5)
              d12 (+ d3 2)
              d13 (- d2 3)
              d14 (- d1 2)]
        :when (and (< 0 d6 10)
                   (< 0 d7 10)
                   (< 0 d9 10)
                   (< 0 d11 10)
                   (< 0 d12 10)
                   (< 0 d13 10)
                   (< 0 d14 10))]
    [d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14]))

(defn p24-1 []
  (parse-long (string/join (last (legal-numbers)))))

(defn p24-2 []
  (parse-long (string/join (first (legal-numbers)))))
