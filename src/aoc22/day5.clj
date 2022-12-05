(ns aoc22.day5
  (:gen-class)
  (:require [clojure.string :as str]))

(defn input-width [input] (+ 1 (str/index-of input "\n")))
(defn input-height [input] (count (re-seq #"\n" input)))

(defn read-column [input, x]
  (let [h (input-height input) w (input-width input)]
    (->>
     (for [y (range h)]
       (first (subs input (+ (* y w) x))))
     (filter #(not= % \space))
     (reverse)
     (into (list)))))

(defn parse-stacks [stacks]
  (->> (range 1 (input-width stacks) 4)
       (map #(read-column stacks %))
       (vec)))

(defn parse-order-line [input]
  (->> (str/split input #" ")
       (map read-string)
       (filter integer?)
       (zipmap [:count :from :to])
       (#(-> %  ;start stack indexing from 0
             (assoc :from (dec (% :from)))
             (assoc :to (dec (% :to)))))))

(defn parse-orders [input]
  (->> (str/split-lines input)
       (map parse-order-line)
       (into (list))
       (reverse)))

(defn move-a-crate [stacks to from]
  (-> stacks
      (assoc to (conj (stacks to) (peek (stacks from))))
      (assoc from (pop (stacks from)))))

(defn move-crates-model-9000 [stacks orders]
  (let [{count :count to :to from :from} orders]
    (loop [count count stacks stacks]
      (if (= 0 count)
        stacks
        (recur (dec count) (move-a-crate stacks to from))))))

(defn move-crates-model-9001 [stacks orders]
  (let [{count :count to :to from :from} orders]
    (-> stacks
        (assoc to (flatten (conj (stacks to) (take count (stacks from)))))
        (assoc from (drop count (stacks from))))))

(defn apply-orders [stacks orders mover-fn]
  (loop [stacks stacks orders orders]
    (if (empty? orders)
      stacks
      (recur
       (mover-fn stacks (peek orders))
       (pop orders)))))

(defn -main []
  (->> (str/split (slurp "resources/day5.txt") #"\n\n")
       (#(apply-orders
          (parse-stacks (% 0))
          (parse-orders (% 1))
          move-crates-model-9001)) ; use move-crates-mode-9000 for part 1
       (map first)
       (apply str)))
