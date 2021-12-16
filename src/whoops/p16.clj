(ns whoops.p16
  (:require
   [clojure.string :as string]
   [whoops.common :refer [read-input]]))

(def hex-map
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn parse-hex-string [s]
  (->> s
       (map hex-map)
       string/join))

(defn binstr->long [s]
  (Long/valueOf s 2))

(declare parse-packet)

(defn parse-header [packet]
  (let [version-str (subs packet 0 3)
        type-str (subs packet 3 6)
        payload (subs packet 6)]
    [(binstr->long version-str) (binstr->long type-str) payload]))

(defn parse-literal [payload]
  (loop [binstr ""
         payload payload]
    (let [continue? (= \1 (first payload))
          bin (subs payload 1 5)
          remainder (subs payload 5)]
      (if continue?
        (recur (str binstr bin) remainder)
        [(binstr->long (str binstr bin)) remainder]))))

(defn parse-operator-by-length [payload]
  (let [len  (binstr->long (subs payload 0 15))
        payload (subs payload 15)
        payload-len (count payload)]
    (loop [packets []
           payload payload]
      (let [[packet remainder] (parse-packet payload)
            packets (conj packets packet)
            consumed (- payload-len (count remainder))]
        (if (= consumed len)
          [{:operator-type :length
            :operator-length len
            :packets packets} remainder]
          (recur packets remainder))))))

(defn parse-operator-by-count [payload]
  (let [packet-count (binstr->long (subs payload 0 11))
        payload (subs payload 11)]
    (loop [packets []
           payload payload
           cnt 0]
      (if (= cnt packet-count)
        [{:operator-type :count
          :operator-count packet-count
          :packets packets} payload]
        (let [[packet remainder] (parse-packet payload)]
          (recur (conj packets packet) remainder (inc cnt)))))))
(defn parse-operator [payload]
  (let [total-length? (= \0 (first payload))]
    (if total-length?
      (parse-operator-by-length (subs payload 1))
      (parse-operator-by-count (subs payload 1)))))

(defn version-sum [packet]
  (if-let [packets (get-in packet [:payload :packets])]
    (+ (:version packet) (reduce #(+ %1 (version-sum %2)) 0 packets))
    (:version packet)))

(defn parse-packet [packet]
  (let [[version packet-type payload] (parse-header packet)
        [payload remainder] (if (= 4 packet-type) (parse-literal payload) (parse-operator payload))]
    [{:version version
      :type packet-type
      :payload payload}
     remainder]))

(defn get-packets [packet]
  (get-in packet [:payload :packets]))

(defmulti eval-packet :type)

(defmethod eval-packet 0 [packet]
  (->> packet
       get-packets
       (map eval-packet)
       (reduce +)))

(defmethod eval-packet 1 [packet]
  (->> packet
       get-packets
       (map eval-packet)
       (reduce *)))

(defmethod eval-packet 2 [packet]
  (->> packet
       get-packets
       (map eval-packet)
       (apply min)))

(defmethod eval-packet 3 [packet]
  (->> packet
       get-packets
       (map eval-packet)
       (apply max)))

(defmethod eval-packet 4 [packet]
  (:payload packet))

(defmethod eval-packet 5 [packet]
  (let [[a b] (map eval-packet (get-packets packet))]
    (if (> a b)
      1
      0)))

(defmethod eval-packet 6 [packet]
  (let [[a b] (map eval-packet (get-packets packet))]
    (if (< a b)
      1
      0)))

(defmethod eval-packet 7 [packet]
  (let [[a b] (map eval-packet (get-packets packet))]
    (if (= a b)
      1
      0)))


(defn p16-1 []
  (-> (read-input "16-1")
      first
      parse-hex-string
      parse-packet
      first
      version-sum))

(defn p16-2 []
  (-> (read-input "16-1")
      first
      parse-hex-string
      parse-packet
      first
      eval-packet))
