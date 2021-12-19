(ns whoops.p18
  (:require
   [clojure.edn :as edn]
   [clojure.zip :as zip]
   [whoops.common :refer [read-input]]))

(def day18-data (->> (read-input "18-1")
                     (map edn/read-string)))
(defn depth [zipper]
  (count (zip/path zipper)))

(defn find-right [base-zipper]
  (loop [zipper (-> base-zipper zip/next zip/next zip/next) ;;advance over current form
           cnt 3]
    (cond
      (zip/end? zipper) [base-zipper 0]
      (nil? zipper) [base-zipper 0]
      (number? (zip/node zipper)) [zipper cnt]
      :else (recur (zip/next zipper) (inc cnt)))))

(defn find-left [base-zipper]
  (loop [zipper (-> base-zipper zip/prev) ;;advance over current form
           cnt -1]
      (cond
        (nil? zipper) [base-zipper 0]
        (number? (zip/node zipper)) [zipper cnt]
        :else (recur (zip/prev zipper) (dec cnt)))))

(defn return [base-zipper cnt]
  (let [nav-fn (if (pos? cnt) zip/prev zip/next)
        cnt (Math/abs cnt)]
    (->> base-zipper
         (iterate nav-fn)
         (take (inc cnt))
         last)))

(defn inc-right [zipper value]
  (let [[loc cnt] (find-right zipper)]
    (if (= 0 cnt)
      zipper
      (-> loc
          (zip/edit #(+ value %))
          (return cnt)))))

(defn inc-left [zipper value]
  (let [[loc cnt] (find-left zipper)]
    (if (= 0 cnt)
      zipper
      (-> loc
          (zip/edit #(+ value %))
          (return cnt)))))

(defn explode [zipper]
  (let [[left right] (zip/node zipper)]
    (-> zipper
        (inc-left left)
        (inc-right right)
        (zip/replace 0))))

(defn split [zipper]
  (let [node (zip/node zipper)
        left (long (Math/floor (/ node 2)))
        right (long (Math/ceil (/ node 2)))]
    (zip/replace zipper [left right])))

(defn needs-exploded? [zipper]
  (and (vector? (zip/node zipper)) (= 4 (depth zipper))))

(defn needs-split? [zipper]
  (let [node (zip/node zipper)]
    (and (number? node) (>= node 10))))

(defn reduce-explode [form]
  (loop [zipper (zip/vector-zip form)]
    (cond
      (zip/end? zipper) form
      (needs-exploded? zipper) (-> zipper explode zip/root)
      :else (recur (zip/next zipper)))))

(defn reduce-split [form]
  (loop [zipper (zip/vector-zip form)]
    (cond
      (zip/end? zipper) form
      (needs-split? zipper) (-> zipper split zip/root)
      :else (recur (zip/next zipper)))))

(defn reduce-once [form]
  (let [exploded (reduce-explode form)]
    (if (identical? exploded form)
      (reduce-split form)
      exploded)))

(defn reduce-form [form]
  (loop [form form]
    (let [new-form (reduce-once form)]
      (if (identical? new-form form)
        new-form
        (recur new-form)))))

(defn add-forms [form1 form2]
  (let [r (reduce-form [form1 form2])]
    r))
(defn magnitude [form]
  (if (number? form)
    form
    (+ (* 3 (magnitude (first form)))
       (* 2 (magnitude (second form))))))

(defn p18-1 []
  (->> day18-data
       (reduce add-forms)
       magnitude))

(defn p18-2 []
  (reduce max
          (for [x day18-data
                y day18-data]
            (magnitude (add-forms x y)))))
