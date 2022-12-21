(ns aoc22.day17
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def rocks
  {:- [[2 3] [3 3] [4 3] [5 3]]
   :+ [[2 4] [3 4] [4 4] [3 3] [3 5]]
   :L [[2 3] [3 3] [4 3] [4 4] [4 5]]
   :I [[2 3] [2 4] [2 5] [2 6]]
   :# [[2 3] [3 3] [2 4] [3 4]]})

(defn translate [rock [dx dy]] (set (map #(mapv + % [dx dy]) rock)))

(defn new-rock [n h]
  (-> (vec (keys rocks))
      (get (mod n 5))
      (rocks)
      (translate [0 h])))

(def initial-state
  {:n 0
   :h 0
   :stopped-rocks #{}
   :jets (slurp "resources/day17.txt")
   :jet-idx 0
   :rock (new-rock 0 0)
   :stopped false})

(defn out-of-bounds? [rock]
  (let [x-coords (map #(% 0) rock)
        y-coords (map #(% 1) rock)]

    (or (< (apply min y-coords) 0)
        (< (apply min x-coords) 0)
        (> (apply max x-coords) 6))))

(defn collision? [stopped-rocks rock]
  (or (not-empty (set/intersection stopped-rocks rock))
      (out-of-bounds? rock)))

(defn next-jet [state]
  (let [{jet-idx :jet-idx jets :jets} state]
    (assoc state :jet-idx
           (if (= jet-idx (- (count jets) 1))
             0
             (inc jet-idx)))))

(defn process-jet [state]
  (let [dx (if (= \< (get-in state [:jets (state :jet-idx)])) -1 1)
        translated-rock (translate (state :rock) [dx 0])
        new-state (next-jet state)]
    (if (collision? (state :stopped-rocks) translated-rock)
      new-state
      (assoc new-state :rock translated-rock))))

(defn process-fall [state]
  (let [translated-rock (translate (state :rock) [0 -1])]
    (if (collision? (state :stopped-rocks) translated-rock)
      (let [h (max (+ 1 (apply max (map #(% 1) (state :rock)))) (state :h))
            n (inc (state :n))]
        (-> state
            (assoc :stopped true)
            (assoc :stopped-rocks
                   (set/union (state :stopped-rocks)
                              (state :rock)))
            (assoc :n n)
            (assoc :h h)
            (assoc :rock (new-rock n h))))

      (assoc state :rock translated-rock))))

(defn process-one-rock [state]
  (loop [state (assoc state :stopped false)]
    (if (state :stopped)
      state
      (->> state
           (process-jet)
           (process-fall)
           (recur)))))

(defn process-n-rocks [n]
  (loop [state initial-state]
    (if (= (state :n) n)
      state
      (recur (process-one-rock state)))))

(defn process-rocks-until-height [h]
  (loop [state initial-state]
    (if (<= h  (state :h))
      state
      (recur (process-one-rock state)))))

(comment "By printing the rock-pattern for 7000 rocks we notice 
          that the the pattern from heights 
          425 to 3072 is repeating ;)")
(defn print-7000-rocks []
  (println
   (->> (loop [stopped-rocks ((process-n-rocks 7000) :stopped-rocks)
               visu (vec (repeat 12000 [\. \. \. \. \. \. \.]))]

          (if (empty? stopped-rocks)
            visu
            (recur (rest stopped-rocks)
                   (assoc-in visu [((first stopped-rocks) 1) ((first stopped-rocks) 0)] \#))))
        (map str/join)
        (str/join "\n"))))

;(def rocks-until-first-cycle (get (process-rocks-until-height 425) :n))
;(def rocks-per-cycle (- (get (process-rocks-until-height 3072) :n) rocks-until-first-cycle))
;(def n-cycles (bigint (/ (- 1000000000000N rocks-until-first-cycle) rocks-per-cycle)))
;(def height-in-cycles (* n-cycles (- 3072 425)))
;(def rocks-after-cycles (mod (- 1000000000000N rocks-until-first-cycle) rocks-per-cycle))
;(def height-out-of-cycles ((process-n-rocks (+ rocks-until-first-cycle rocks-after-cycles)) :h))

;(defn -main []
;  {:part1 ((process-n-rocks 2022) :h)
;   :part2 (+ height-out-of-cycles height-in-cycles)})
