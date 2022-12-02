(ns aoc22.main 
  (:require [aoc22.day1 :as day1]
            [aoc22.day2 :as day2]))

(defn -main [which]
  (let [day (Integer. which)]
    (case day
      1 (println "Exercise 1:" (day1/-main))
      2 (println "Exercise 2:" (day2/-main)) 
      )))