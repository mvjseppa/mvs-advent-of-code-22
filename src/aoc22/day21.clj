(ns aoc22.day21
  (:gen-class)
  (:require [clojure.string :as str]))

(defn parse-node [tokens]
  (if (= 1 (count tokens))
    {:val (read-string (tokens 0)) 
     :op nil 
     :children []}
    
    {:val nil 
     :op ({"+" + "-" - "*" * "/" quot} (tokens 1)) 
     :children [(keyword (tokens 0)) (keyword (tokens 2))]}))

(def monkeys (->> (slurp "resources/day21.txt")
     (str/split-lines)
     (reduce
      (fn [acc line] (->> line
                          (re-seq #"(?:[a-z]{4}|[\+\-\*\/]|\d+)")
                          (vec)
                          (#(assoc acc 
                                   (keyword (first %)) 
                                   (parse-node (vec (rest %)))))))
      {})))

(defn monkey-value [monkey-name] 
  (let [monkey (monkeys monkey-name)
        {val :val
         op :op
         children :children} monkey] 
    (or val (apply op (map monkey-value children)))))

(defn -main []
  {:part1 (monkey-value :root)
   :part2 "???"})
