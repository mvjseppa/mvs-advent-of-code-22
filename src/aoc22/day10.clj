(ns aoc22.day10
  (:gen-class)
  (:require [clojure.string :as str]))

(defn run-cmd [state cmd]
  (let [cmd-parts (str/split cmd #" ")]
    (case (first cmd-parts)
      "noop" (assoc state :history
                    (conj
                     (state :history)
                     (state :register)))

      "addx" (-> state
                 (run-cmd "noop")
                 (run-cmd "noop")
                 (assoc :register
                        (+
                         (read-string (cmd-parts 1))
                         (state :register)))))))

(defn run-prog []
  (->> (slurp "resources/day10.txt")
       (str/split-lines)
       (#(loop [cmds % state {:register 1 :history []}]
           (if (empty? cmds)
             state
             (recur (rest cmds)
                    (run-cmd state (first cmds))))))))

(defn signal-strengths [indexes state]
  (map #(* ((state :history) (dec %)) %) indexes))

(defn part1 [state]
  (->> state
       (signal-strengths [20 60 100 140 180 220])
       (reduce +)))

(defn part2 [state]
  (->> (state :history)
       (keep-indexed #(if (< (abs(- (mod %1 40) %2)) 2) "#" "."))
       (apply str)
       (re-seq #".{1,40}")
       (str/join "\n")))


(defn -main []
  (->> (run-prog)
       (#(hash-map :part1 (part1 %)
                   :part2 (println (part2 %))))))
