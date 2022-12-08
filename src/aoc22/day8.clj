(ns aoc22.day8
  (:gen-class)
  (:require [clojure.string :as str]))

(defn parse-tree-map []
  (let [tree-map (->> (slurp "resources/day8.txt")
                      (str/split-lines)
                      (map #(->> (re-seq #".{1,1}" %)
                                 (map read-string)
                                 (vec)))
                      (vec))]
    {:original tree-map
     :transpose (apply mapv vector tree-map)}))

(defn tree-visible-in-row? [tree-row idx]
  (or (every? #(> (tree-row idx) %) (subvec tree-row 0 idx))
      (every? #(> (tree-row idx) %) (subvec tree-row (+ 1 idx)))))

(defn tree-visible [x y trees]
  (if
   (or
    (tree-visible-in-row? ((trees :original) y) x)
    (tree-visible-in-row? ((trees :transpose) x) y))
    1 0))

(defn mark-trees [marker-fn trees]
  (map-indexed
   (fn [y row]
     (map-indexed
      (fn [x _] (marker-fn x y trees))
      row))
   (trees :original)))

(defn scenic-score-one-direction [line-of-sight]
  (or (first (keep-indexed
              #(when (<= (first line-of-sight) %2) (inc %1))
              (rest line-of-sight))) (count (rest line-of-sight))))

(defn scenic-score-row [tree-row idx]
  (*
   (scenic-score-one-direction (subvec tree-row idx))
   (scenic-score-one-direction (reverse (subvec tree-row 0 (inc idx))))))

(defn scenic-score [x y trees]
  (*
   (scenic-score-row ((trees :original) y) x)
   (scenic-score-row ((trees :transpose) x) y)))

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
    {:part1 (part1 tree-map) :part2 (part2 tree-map)}))

