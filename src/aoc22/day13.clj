(ns aoc22.day13
  (:gen-class)
  (:require [clojure.string :as str]))

(def packet-list
  (->> (slurp "resources/day13.txt")
       (str/split-lines)
       (remove str/blank?)
       (map read-string)))

(def packet-list-with-markers
  (concat packet-list [[[2]] [[6]]]))

(def packet-pairs
  (partition 2 packet-list))

(defn- is-divider? [packet]
  (or (= packet [[2]]) (= packet [[6]])))

(defn- adv-compare-lists [l r cmp-fn]
  (loop [l l r r]
    (cond
      (every? empty? [l r]) 0
      (empty? l) 1
      (empty? r) -1
      :else  (let [c (cmp-fn (first l) (first r))]
                  (if (not= 0 c) c (recur (rest l) (rest r)))))))

(defn adv-compare [l r]
  (cond
    (every? int? [l r])        (- r l)
    (every? vector? [l r])     (adv-compare-lists l r adv-compare)
    (and (vector? l) (int? r)) (adv-compare l [r])
    (and (int? l) (vector? r)) (adv-compare [l] r)
    :else                      (throw (Exception. "epic fail"))))

(defn -main []
  {:part1 (->> packet-pairs
               (keep-indexed
                (fn [i v]
                  (when (pos-int? (apply adv-compare v)) (inc i))))
               (reduce +))
   :part2 (->> packet-list-with-markers
               (sort-by identity adv-compare)
               (reverse)
               (keep-indexed (fn [i v] (when (is-divider? v) (inc i))))
               (reduce *))})

;----- tests -----
(def positive-samples
  [(adv-compare -2 -1)
   (adv-compare [1,1,3,1,1] [1,1,5,1,1])
   (adv-compare [[1],[2,3,4]] [[1],4])
   (adv-compare [[4,4],4,4] [[4,4],4,4,4])
   (adv-compare [] [3])])

(def negative-samples
  [(adv-compare 1 -1)
   (adv-compare [9] [[8,7,6]])
   (adv-compare [7,7,7,7] [7,7,7])
   (adv-compare [[[]]] [[]])
   (adv-compare [1,[2,[3,[4,[5,6,7]]]],8,9] [1,[2,[3,[4,[5,6,0]]]],8,9])])

(map #(assert (pos-int? %)) positive-samples)
(map #(assert (neg-int? %)) negative-samples)
