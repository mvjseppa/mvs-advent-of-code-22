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

(defn collision? [[x y] sand] (or (= y y-max) (get-in sand [y x])))

(defn move-particle [[x y] sand]
  (or (first (filter
              #(not (collision? % sand))
              [[x (inc y)]
               [(dec x) (inc y)]
               [(inc x) (inc y)]]))
      [x y]))

(defn process-particle [sand]
  (loop [[x y] [500 0] prev nil]
    (cond
      (= [x y] prev) [(assoc sand y (conj (sand y) x)) [x y]]
      :else (recur
             (move-particle [x y] sand)
             [x y]))))

(defn count-sand [sand] (reduce + (map count sand)))

(defn -main []
  (loop [sand initial-obstacles n 0]
    (let [[new-sand particle] (process-particle sand)]
      ;(when (zero? (mod n 100)) (println n))
      (if (or (nil? particle) (= 0 (last particle)))
        (- (count-sand new-sand) (count-sand initial-obstacles))
        (recur new-sand (inc n))))))

(time (-main))
