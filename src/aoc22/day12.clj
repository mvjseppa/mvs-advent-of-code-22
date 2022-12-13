(ns aoc22.day12
  (:gen-class)
  (:require [clojure.string :as str]))

(def terrain (->>
              (slurp "resources/day12.txt")
              (str/split-lines)))

(defn find-in-terrain [c]
  (->>
   (map #(str/index-of % c) terrain)
   (keep-indexed (fn [i v] (when v [v i])))
   (first)))

(defn update-cost [costs [x y] value]
  (assoc-in costs [y x] value))

(def all-directions '(:up :down :left :right))

(defn initial-state [start]
  {:pos start
   :end (find-in-terrain \E)
   :costs (update-cost
           (->> (count (terrain 0))
                (#(repeat % 1000000))
                (vec)
                (repeat (count terrain))
                (vec))
           start 0)
   :queue []
   :visited #{}})

(def dir->vec {:up [0 -1] :right [1 0] :down [0 1] :left [-1 0]})
(defn update-pos [pos dir] (mapv + pos (dir->vec dir)))

(defn char-height [c]
  (case c
    \S 0
    \E 25
    \# 1000
    (- (int c) (int \a))))

(defn height-at [[x y]] (-> (get-in terrain [y x]) (or \#) (char-height)))

(defn current-cost [state]
  (let [[x y] (state :pos)] (get-in state [:costs y x])))

(defn update-neighbor [[x y] state]
  (let [new-cost (+ (current-cost state) 1)
        old-cost (get-in state [:costs y x])]
    (-> state
        (assoc :queue (if (some #(= % [x y]) (state :queue)) (state :queue) (conj (state :queue) [x y])))
        (assoc-in [:costs y x] (min new-cost old-cost)))))

(defn sorted-unvisited-neighbors [state]
  (let [pos (state :pos)]
    (->>  all-directions
          (map #(update-pos pos %))
          (filter (fn [it] (and (not (some #(= it %) (state :visited)))
                                (<= (- (height-at it) (height-at pos)) 1))))
          (sort-by #(- (height-at %) (height-at pos))))))

(defn update-neighbors [state]
  (loop [neighbors (sorted-unvisited-neighbors state)
         state state]

    (if (empty? neighbors)
      state
      (recur
       (rest neighbors)
       (update-neighbor (first neighbors) state)))))

(defn move [state]
  (let [[x y] (state :pos)]
    ;(println (state :queue))
    (-> state
        (assoc :pos (if (empty? (state :queue))
                      (state :end)
                      (first (state :queue))))
        (assoc :visited (conj (state :visited) [x y]))
        (assoc :queue (vec (rest (state :queue)))))))

(defn dijkstra [state]
  (println (state :pos))
  (loop [state state]
    (if (= (state :pos) (state :end))
      state
      (->> state (update-neighbors) (move) (recur)))))

(defn find-all-a []
  (->> terrain
       (map #(keep-indexed (fn [i v] (when (or (= v \a) (= v \S)) i)) %))
       (keep-indexed (fn [i v] (map (fn [i2] [i2 i]) v)))
       (apply concat)))

(defn -main []
  (->> (find-all-a)
       (map initial-state)
       (map #(-> (dijkstra %)
                 (get :costs)
                 (get-in (reverse (% :end)))))
       (apply min)))
