(ns aoc22.day14
  (:gen-class)
  (:require [clojure.string :as str]))

(defn read-point [s] (read-string (str "[" s "]")))

(defn sort-line [line]
  (let [[x0 x1] (vec (sort (map first line)))
        [y0 y1] (vec (sort (map second line)))]
    [[x0 y0] [x1 y1]]))

(def all-lines
  (->> (slurp "resources/day14.txt")
       (str/split-lines)
       (map #(str/split % #" -> "))
       (map #(map read-point %))
       (map #(map vec (partition 2 1 %)))
       (apply concat)
       (map sort-line)))

all-lines

(def y-max
  (->> (flatten all-lines)
       (partition 2)
       (map last)
       (apply max)
       (+ 2)))

(defn point-on-line? [[xp yp] [[x0 y0] [x1 y1]]]
  ;(println [x0 y0] [x1 y1])
  (cond
    (= x0 x1) (and (= xp x0) (<= y0 yp y1))
    (= y0 y1) (and (= yp y0) (<= x0 xp x1))
    :else (throw (Exception. "epic fail"))))

(defn not-reserved? [[x y] sand]
  (not (or
        (get-in sand [x y])
        (= y y-max)
        (some #(point-on-line? [x y] %)  all-lines))))

(defn move-particle [[x y] sand]
  (or (first (filter
              #(not-reserved? % sand)
              [[x (inc y)]
               [(dec x) (inc y)]
               [(inc x) (inc y)]]))
      [x y]))

(move-particle [500 8] [])

all-lines

(defn process-particle [sand]
  (loop [[x y] [500 0] prev nil]
    ;(println [x y] prev)
    (cond
      ;(> y y-max) nil
      (= [x y] prev) [(assoc-in sand [x y] true) [x y]]
      :else (recur
             (move-particle [x y] sand)
             [x y]))))

(process-particle [])

(defn count-sand [sand] (reduce + (map count (vals sand))))

(defn -main []
  (loop [sand {}]
    (let [[new-sand particle] (process-particle sand)]
      (println particle)
      (if (or (nil? particle) (= 0 (last particle)))
        (count-sand new-sand)
        (recur new-sand)))))

(-main)

(vec (sort [4 2]))

(point-on-line? [3 9] [[3 5] [3 10]])
(point-on-line? [3 9] [[3 10] [3 5]])
(point-on-line? [[3 2] [12 2]] [3 2])
(point-on-line? [[3 3] [12 2]] [3 2])

(point-on-line? [[3 5] [3 10]] [3 11])
(point-on-line? [[3 2] [12 2]] [1 2])
(point-on-line? [[3 2] [12 2]] [0 2])
