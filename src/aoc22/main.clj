(ns aoc22.main
  (:require [aoc22.day1]
            [aoc22.day2]
            [aoc22.day3]
            [aoc22.day4]
            [aoc22.day5]
            [aoc22.day6]
            [aoc22.day7]
            [aoc22.day8]
            [aoc22.day9]
            [aoc22.day10]
            [aoc22.day11]
            [aoc22.day12]
            [aoc22.day13]
            [aoc22.day14]
            [aoc22.day15]))

(defn -main [which]
  (let [day (Integer. which)
        puzzle-fn (format "aoc22.day%s/-main" day)
        result (apply (resolve (symbol puzzle-fn)) [])]
    (println (format "Puzzle %s: %s" day result))))
