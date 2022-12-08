(ns aoc22.day8
  (:gen-class)
  (:require [clojure.string :as str]))

(defn parse-tree-map []
  (->> (slurp "resources/day8.txt")
       (str/split-lines)
       (map #(->> (re-seq #".{1,1}" %)
                  (map read-string)
                  (vec)))
       (vec)))

(defn column [n v] (vec (flatten (map #(% n) v))))

(defn lines-of-sight [row idx]
  [(subvec row idx)
   (vec (reverse (subvec row 0 (inc idx))))])

(defn tree-visible-one-direction [line-of-sight]
  (every? #(> (first line-of-sight) %) (rest line-of-sight)))

(defn tree-visible-in-row? [row idx]
  (->> (lines-of-sight row idx)
       (map tree-visible-one-direction)
       (reduce #(or %1 %2))))

(defn tree-visible [x y trees]
  (if (or (tree-visible-in-row? (trees y) x)
          (tree-visible-in-row? (column x trees) y))
    1
    0))

(defn mark-trees [marker-fn trees]
  (map-indexed
   (fn [y row] (map-indexed
                (fn [x _] (marker-fn x y trees))
                row))
   trees))

(defn scenic-score-one-direction [line-of-sight]
  (or (first (keep-indexed
              #(when (<= (first line-of-sight) %2) (inc %1))
              (rest line-of-sight)))
      (count (rest line-of-sight))))

(defn scenic-score-row [row idx]
  (->> (lines-of-sight row idx)
       (map scenic-score-one-direction)
       (reduce *)))

(defn scenic-score [x y trees]
  (* (scenic-score-row (trees y) x)
     (scenic-score-row (column x trees) y)))

(defn part1 [tree-map]
  (->> (mark-trees tree-visible tree-map)
       (flatten)
       (reduce +)))

(defn part2 [tree-map]
  (->> (mark-trees scenic-score tree-map)
       (flatten)
       (apply max)))

(defn -main []
  (let [tree-map (parse-tree-map)]
    {:part1 (part1 tree-map)
     :part2 (part2 tree-map)}))
