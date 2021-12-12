(ns whoops.p12
  (:require
   [whoops.common :refer [read-input]]
   [clojure.string :as string]
   [clojure.set :as set]))

(defn update-graph [graph [edge1 edge2]]
  (let [edges1 (get graph edge1 #{})
        edges2 (get graph edge2 #{})]
    (-> graph
        (assoc edge1 (conj edges1 edge2))
        (assoc edge2 (conj edges2 edge1)))))

(defn parse-input [input]
  (->> input
       (map #(string/split % #"-"))
       (reduce update-graph {})))

(def day12-input (parse-input (read-input "12-1")))

(defn is-lower? [s]
  (= s (string/lower-case s)))

(defn explore
  ([graph] (explore graph "start" [] #{}))
  ([graph node path visited]
   (let [connections (set/difference (graph node) visited)
         visited (if (is-lower? node) (conj visited node) visited)
         path (conj path node)]
     (if (= "end" node)
       [path]
       (mapcat #(explore graph % path visited) connections)))))

(defn explore-advanced
  ([graph] (explore-advanced graph "start" [] #{} false))
  ([graph node path visited twice?]
   (let [twice? (if (visited node) true twice?)
         connections (set (remove #(= "start" %) (graph node)))
         connections (if twice? (set/difference connections visited) connections)
         visited (if (is-lower? node) (conj visited node) visited)
         path (conj path node)]
     (if (= "end" node)
       [path]
       (mapcat #(explore-advanced graph % path visited twice?) connections)))))

(defn p12-1 []
  (count (explore day12-input)))

(defn p12-2 []
  (count (explore-advanced day12-input)))
