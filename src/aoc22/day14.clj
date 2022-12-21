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

(defn line-to-points [[[x0 y0] [x1 y1]]]
  (cond
    (= x0 x1) (map (fn [y] [x0 y]) (range y0 (inc y1)))
    (= y0 y1) (map (fn [x] [x y0]) (range x0 (inc x1)))))

(def y-max
  (->> (flatten all-lines)
       (partition 2)
       (map last)
       (apply max)
       (+ 2)))

(def initial-obstacles
  (->> all-lines
       (map line-to-points)
       (apply concat)
       (reduce 
        (fn [acc [x y]] (assoc acc y (conj (acc y) x)))
        (vec (repeat (inc y-max) #{})) 
        )))

(defn collision? [[x y] obstacles] (or (= y y-max) (get-in obstacles [y x])))

(defn move-particle [[x y] obstacles]
  (or (first (filter
              #(not (collision? % obstacles))
              [[x (inc y)]
               [(dec x) (inc y)]
               [(inc x) (inc y)]]))
      [x y]))

(defn process-particle [obstacles]
  (loop [[x y] [500 0] prev nil]
    (cond
      (= [x y] prev) [(assoc obstacles y (conj (obstacles y) x)) [x y]]
      :else (recur
             (move-particle [x y] obstacles)
             [x y]))))

(defn count-points [obstacles] (reduce + (map count obstacles)))

(defn -main []
  (loop [obstacles initial-obstacles]
    (let [[new-obstacles particle] (process-particle obstacles)]
      (if (or (nil? particle) (= 0 (last particle)))
        (- (count-points new-obstacles) (count-points initial-obstacles))
        (recur new-obstacles)))))
