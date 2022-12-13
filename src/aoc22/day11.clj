(ns aoc22.day11
  (:gen-class)
  (:require [clojure.string :as str]))

(defn parse-operation [input]
  (->> (str/split input #" ")
       (take-last 3)
       (apply (fn [arg1 op arg2]
                (fn [old] ((resolve (symbol op))
                           (if (= arg1 "old") old (read-string arg1))
                           (if (= arg2 "old") old (read-string arg2))))))))

(defn read-last-word [s] (read-string (last (str/split s #"[\s:]"))))

(defn parse-items [s] (->> (str/split s #"[\s,]")
                           (remove str/blank?)
                           (drop 2)
                           (map #(read-string %))
                           (vec)))

(defn parse-monkey [input]
  (->> (str/split-lines input)
       ((fn [monkey] {:id (read-last-word (monkey 0))
                      :items (parse-items (monkey 1))
                      :operation (parse-operation (monkey 2))
                      :divisor (read-last-word (monkey 3))
                      :on-true (read-last-word (monkey 4))
                      :on-false (read-last-word (monkey 5))
                      :inspection-count 0}))))

(def initial-monkeys
  (->> (slurp "resources/day11.txt")
       (#(str/split % #"\n\n"))
       (map parse-monkey)
       (vec)))

(def divisor-product
  (->> initial-monkeys
       (map #(get % :divisor))
       (reduce *)))

(defn update-worry-levels [monkey worry-divisor]
  (->> (monkey :items)
       (map #(-> ((monkey :operation) %)
                 (mod divisor-product)
                 (/ worry-divisor)))
       (vec)
       (assoc monkey :items)))

(defn count-inspections [monkey]
  (->> (count (get monkey :items))
       (+ (get monkey :inspection-count))))

(defn throw-item [monkeys from-idx to-idx]
  (let [from-items (get-in monkeys [from-idx :items])
        to-items (get-in monkeys [to-idx :items])]

    (if (empty? from-items)
      monkeys
      (-> monkeys
          (assoc-in [from-idx :items] (vec (rest from-items)))
          (assoc-in [to-idx :items] (conj to-items (first from-items)))))))

(defn throw-items [monkeys idx]
  (loop [items (get-in monkeys [idx :items]) monkeys monkeys]
    (if (empty? items)
      monkeys
      (recur (rest items)
             (throw-item monkeys
                         idx
                         (if (zero? (mod (first items) (get-in monkeys [idx :divisor])))
                           (get-in monkeys [idx :on-true])
                           (get-in monkeys [idx :on-false])))))))

(defn process-turn [monkeys idx worry-divisor]
  (-> monkeys
      (assoc idx (update-worry-levels (monkeys idx) worry-divisor))
      (assoc-in [idx :inspection-count] (count-inspections (monkeys idx)))
      (throw-items idx)))

(defn process-round [monkeys worry-divisor]
  (loop [idx 0 monkeys monkeys]
    (if (>= idx (count monkeys))
      monkeys
      (recur (inc idx)
             (process-turn monkeys idx worry-divisor)))))

(defn process-n-rounds [n monkeys worry-divisor]
  (loop [n n monkeys monkeys]
    (if (zero? n)
      monkeys
      (recur (dec n) (process-round monkeys worry-divisor)))))

(def end-state-part1 (process-n-rounds 20 initial-monkeys 3))
(def end-state-part2 (process-n-rounds 10000 initial-monkeys 1))

(defn count-monkey-business [monkeys]
  (->> monkeys
       (map #(get % :inspection-count))
       (sort #(compare %2 %1))
       (take 2)
       (reduce *)))

(defn -main []
  {:part1 (count-monkey-business end-state-part1)
   :part2 (count-monkey-business end-state-part2)})
