(ns whoops.advent-2021
  (:require [clojure.string :as string])
  (:gen-class))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))


(defn read-input [problem]
  (let [file (str "resources/input/" problem "/input")]
    (string/split-lines (slurp file))))


(defn p1-1 []
  (let [data (map #(Integer/parseInt %) (read-input "1-1"))
        reducer (fn [[cnt lst] nxt]
                  (if (> nxt lst)
                    [(inc cnt) nxt]
                    [cnt nxt]))]
    (first (reduce reducer [0 Integer/MAX_VALUE] data))))

(defn p1-2 []
  (let [data (map #(Integer/parseInt %) (read-input "1-1"))
        reducer (fn [[cnt a b c] d]
                  (cond
                    (not (and a b c d)) [cnt b c d]
                    (> (+ b c d) (+ a b c)) [(inc cnt) b c d]
                    :else [cnt b c d]))]
    (first (reduce reducer [0 nil nil nil] data))))
