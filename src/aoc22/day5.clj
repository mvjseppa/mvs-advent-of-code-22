(ns aoc22.day5
  (:gen-class)
  (:require [clojure.string :as str]))

(defn input-width [input]
  (+ 1 (str/index-of input "\n")))

(defn input-height [input]
  (count (re-seq #"\n" input)))

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

(defn move-crates [stacks orders]
  (let [count (orders 0)
        from (dec (orders 1))
        to (dec (orders 2))]
    (loop [count count stacks stacks]
      (if (= 0 count)
        stacks
        (recur
         (dec count)
         (-> stacks (assoc to (conj (stacks to) (peek (stacks from))))
             (assoc from (pop (stacks from)))))))))

(defn parse-orders [input]
  (->> (str/split-lines input)
       (map #(->> (str/split % #" ")
                  (map read-string)
                  (filter integer?)
                  (vec)))
       (into (list))
       (reverse)))

(defn apply-orders [stacks orders]
  (loop [stacks stacks orders orders]

    (if (empty? orders)
      stacks
      (recur
       (move-crates stacks (peek orders))
       (pop orders)))))

(defn -main []
  (->> (str/split (slurp "resources/day5.txt") #"\n\n")
       (#(apply-orders 
          (parse-stacks (% 0))
          (parse-orders (% 1))))
       (map first)
       (apply str)))

(-main)