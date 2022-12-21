(ns aoc22.day20
  (:gen-class)
  (:require [clojure.string :as str]))

(defn apply-encryption-key [e-key data]
  (into (sorted-map)
        (zipmap (keys data)
                (map (fn [it]
                       (assoc it
                              :s (rem (* e-key (it :v)) (dec (count data)))
                              :v (* e-key (it :v))))
                     (vals data)))))

(def input-data
  (->> (slurp "resources/day20.txt")
       (str/split-lines)
       (keep-indexed (fn [i v] (sorted-map i {:i i :v (read-string v)})))
       (apply merge)))

(def max-idx (apply max (keys input-data)))

(defn swap [i1 i2 data]
  (assoc data i1 (data i2) i2 (data i1)))

(defn current-index [original-index data]
  (->> data
       (filter (fn [v] (= (get-in v [1 :i]) original-index)))
       (first)
       (first)))

(defn zero-value-index [data]
  (->> data
       (filter (fn [v] (= (get-in v [1 :v]) 0)))
       (first)
       (first)))

(defn next-idx [idx step-fn]
  (let [new-idx (step-fn idx)]
    (cond
      (< new-idx 0) max-idx
      (> new-idx max-idx) 0
      :else new-idx)))

(defn mix-one [original-idx data]
  (let [idx (current-index original-idx data)
        steps (get-in data [idx :s])]

    (if (neg? steps)

      (loop [steps steps idx idx data data]
        (if (zero? steps)
          data
          (let [idx2 (next-idx idx dec)]
            (recur (inc steps) idx2 (swap idx idx2 data)))))

      (loop [steps steps idx idx data data]
        (if (zero? steps)
          data
          (let [idx2 (next-idx idx inc)]
            (recur (dec steps) idx2 (swap idx idx2 data))))))))

(defn mix [data mix-times]
  (loop [indexes (flatten (repeat mix-times (keys data)))
         mixed data]

    (when (zero? (mod (or (first indexes) 1) 100)) (println (first indexes)))

    (if (empty? indexes)
      mixed
      (recur (rest indexes)
             (mix-one (first indexes) mixed)))))

(defn nth-from-zero [n data]
  (->> (zero-value-index data)
       (+ (mod n (count data)))
       (#(if (> % (count data)) (- % (count data)) %))
       (#(get-in data [% :v]))))

(defn decrypt-coordinates [data mix-times]
  (let [mixed (mix data mix-times)]
    (->> [1000 2000 3000]
         (map #(nth-from-zero % mixed))
         (reduce +))))

(defn -main []
  {:part1 (decrypt-coordinates (apply-encryption-key 1 input-data) 1)
   :part2 (decrypt-coordinates (apply-encryption-key 811589153 input-data) 10)})


(-main)