(ns aoc22.day18
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def cubes
  (->> (slurp "resources/day18.txt")
       (str/split-lines)
       (map (fn [line] (read-string (str "[" line "]"))))
       (set)))

(def bounds
  {:min (apply mapv min cubes)
   :max (apply mapv max cubes)})

(some identity
      (concat (mapv < [1 3 3] (bounds :min))
              (mapv > [4 3 4] (bounds :max))))

(defn out-of-bounds? [point]
  (some identity
        (concat (mapv < point (bounds :min))
                (mapv > point (bounds :max)))))

(defn neighbor-coordinates [[x y z]]
  #{[(inc x) y z]
    [(dec x) y z]
    [x (inc y) z]
    [x (dec y) z]
    [x y (inc z)]
    [x y (dec z)]})

(defn neighbors-air [cube]
  (if (nil? cube)
    #{}
    (set/difference (neighbor-coordinates cube) cubes)))

(defn is-pocket? [point]
  (loop [path (conj '() point)
         visited #{}]
    (let [pos (peek path)
          neighbors (set/difference (neighbors-air pos) visited)]
      (cond
        (empty? path) true
        (out-of-bounds? pos) false
        (> (count neighbors) 0) (recur
                                 (conj path (first neighbors))
                                 (conj visited pos))
        :else (recur (pop path) (conj visited pos))))))

(defn remove-pockets [neighbors]
  (remove is-pocket? neighbors))

(defn -main []
  {:part1 (->> cubes
               (map #(count (neighbors-air %)))
               (reduce +))

   :part2 (->> cubes
               (map (fn [it] (println it) (count (remove-pockets (neighbors-air it)))))
               (reduce +))})
