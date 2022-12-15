(ns aoc22.day15
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn manhattan-distance [[x0 y0] [x1 y1]]
  (+ (abs (- x0 x1)) (abs (- y0 y1))))

(defn sensor-row-coverage [row-idx reading]
  (let [{[xs ys] :sensor r :radius} reading
        dx (- r (abs (- row-idx ys)))]

    (->> (range (- xs dx) (+ xs dx 1))
         (apply hash-set))))

(sensor-row-coverage 1 {:sensor [8 7] :beacon [2 10]})
(sensor-row-coverage -100 {:sensor [8 7] :beacon [2 10]})

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

beacon-positions

(defn beacons-on-row [row-idx]
  (apply hash-set (for
                   [b beacon-positions
                    :let [x (b 0)]
                    :when (= (b 1) row-idx)]
                    x)))

(beacons-on-row 2000000)

(defn part1 []
  (->> readings
       (map #(sensor-row-coverage 2000000 %))
       (apply set/union)
       (#(set/difference % (beacons-on-row 2000000)))
       (count)))

(time (part1))