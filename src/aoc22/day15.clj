(ns aoc22.day15
  (:gen-class)
  (:require [clojure.string :as str]))

(defn manhattan-distance [[x0 y0] [x1 y1]]
  (+ (abs (- x0 x1)) (abs (- y0 y1))))

(def readings
  (->> (slurp "resources/day15.txt")
       (#(str/split % #"[=,:\n]"))
       (rest)
       (take-nth 2)
       (map read-string)
       (partition 2)
       (partition 2)
       (map (fn [entry] 
              (let [sensor (vec (first entry))
                    beacon (vec (last entry))
                    radius (manhattan-distance sensor beacon)]
                         {:sensor sensor
                         :beacon beacon
                         :radius radius})))))

(def beacon-positions
  (->> readings
       (map #(get % :beacon))
       (apply hash-set)))

(defn row-coverages-one-sensors [row-idx reading]
  (let [{[xs ys] :sensor r :radius} reading
        dx (- r (abs (- row-idx ys)))]
    (when (> dx 0) [(- xs dx) (+ xs dx 1)])))

(defn row-coverages-all-sensors [row-idx]
  (->> readings
       (map #(row-coverages-one-sensors row-idx %))
       (remove nil?)
       (sort)))

(defn traverse-row [y] 
  (loop [x 0 coverages (row-coverages-all-sensors y)] 
    (let [[x1 x2] (first coverages)]
      (cond
        (> x 4000000) nil
        (beacon-positions [x y]) (recur (inc x) coverages)
        (empty? coverages) x
        (<= x1 x (dec x2)) (recur x2 (rest coverages))
        :else (recur x (rest coverages))))))

(defn -main [] 
  (loop [y 0]
    (when (zero? (mod y 10000)) (println y))
    (let [x (traverse-row y)]
      (cond
        (> y 4000000) nil
        (nil? x) (recur (inc y))
        :else (+ y (* 4000000N x))))))

(time (-main))
