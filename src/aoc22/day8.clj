(ns aoc22.day8
  (:gen-class)
  (:require [clojure.string :as str]))

(defn parse-tree-map [s]
  (let [tree-map
        (->> (str/split-lines s)
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

(defn mark-visible-trees [trees]
  (map-indexed
   (fn [y row]
     (map-indexed
      (fn [x _] (tree-visible x y trees))
      row))
   (trees :original)))

(defn -main []
  (->> (slurp "resources/day8.txt")
       (parse-tree-map)
       (mark-visible-trees)
       (flatten)
       (reduce +)))

(-main)

