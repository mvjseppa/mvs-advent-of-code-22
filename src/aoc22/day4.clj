(ns aoc22.day4
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn number-in-interval [number interval]
  (and (>= number (interval 0)) (<= number (interval 1))))

(defn full-overlap? [a b]
  (or (every? #(number-in-interval % a) b)
      (every? #(number-in-interval % b) a)))

(defn any-overlap? [a b]
  (or (some #(number-in-interval % a) b)
      (some #(number-in-interval % b) a)))

(defn parse-assignment [assignment-string]
  (->> (str/split assignment-string #"[,-]")
       (map read-string)
       (partition 2)
       (map vec)))

(defn -main []
  (with-open [reader (io/reader "resources/day4.txt")]
    (->> (line-seq reader)
         (map parse-assignment)
         (filter #(apply any-overlap? %)) ;use full-overlap for task1
         (count))))
