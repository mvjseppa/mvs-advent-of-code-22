(ns aoc22.main 
  (:require [aoc22.day1]
            [aoc22.day2]
            [aoc22.day3]
            [aoc22.day4]
            [aoc22.day5]
            [aoc22.day6]
            [aoc22.day7]))

(defn -main [which]
  (let [day (Integer. which)
        puzzle-fn (format "aoc22.day%s/-main" day)
        result (apply (resolve (symbol puzzle-fn)) [])]
    (println (format "Puzzle %s: %s" day result))))
