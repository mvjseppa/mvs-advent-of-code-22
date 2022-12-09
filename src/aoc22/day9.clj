(ns aoc22.day9
  (:gen-class)
  (:require [clojure.string :as str]))

(def direction->vec {:U [0 1] :D [0 -1] :L [-1 0] :R [1 0]})

(defn normalize [i] (if (= 0 i) 0 (/ i (abs i))))

(defn update-segment [prev-seg seg]
  (let [diff (mapv - prev-seg seg)]
    (mapv + seg (if (> (apply max (map abs diff)) 1)
                (vec (map normalize diff))
                [0 0]))))

(defn follow-head [rope]
  (loop [rope rope idx 1]
    (if (>= idx (count rope))
      rope
      (recur
       (assoc rope idx (update-segment (rope (dec idx)) (rope idx)))
       (inc idx)))))

(defn update-rope [move rope]
  (->> (direction->vec move)
       (mapv + (first rope))
       (assoc rope 0)
       (follow-head)))

(defn tail-positions [rope moves]
  (loop [moves moves rope rope tails [(last rope)]]
    (if (empty? moves)
      tails
      (let [updated-rope (update-rope (first moves) rope)]
        (recur
         (rest moves)
         updated-rope
         (conj tails (last updated-rope)))))))

(defn parse-repetitions [move-str]
  (->> (str/split move-str #" ")
       (#(repeat (read-string (% 1))
                 (keyword (% 0))))))

(defn parse-moves []
  (->> (slurp "resources/day9.txt")
       (str/split-lines)
       (map parse-repetitions)
       (flatten)
       (vec)))

(defn -main []
  (->> (parse-moves)
       (tail-positions (vec (repeat 10 [0 0])))
       (set)
       (count)))
