(ns aoc22.day6
  (:gen-class))

(def message (slurp "resources/day6.txt"))

(defn is-marker? [snippet] (= (count snippet) (count(set snippet))))

(defn find-marker [message size]
  (loop [i 0]
    (if (is-marker? (subs message i (+ i size)))
      (+ i size)
      (recur (inc i)))))

(defn -main []
  (find-marker message 14)) ;size 4 for task 1
