(ns aoc22.day7
  (:gen-class)
  (:require [clojure.string :as str]))

(defn change-dir [path cmd]
  (let [target-dir (subs (str/trim cmd) 3)]
    (if (= target-dir "..")
      (pop path)
      (conj path (keyword target-dir)))))

(defn ls-data [ls-row]
  (let [parts (str/split ls-row #" ")]
    {:path (keyword (parts 1))
     :payload (if (= (parts 0) "dir")
                {}
                (read-string (parts 0)))}))

(defn process-ls [ls-output node]
  (loop [files ls-output node node]
    (if (empty? files)
      node
      (recur
       (next files)
       (-> (first files)
           (ls-data)
           (#(assoc node (% :path) (% :payload))))))))

(defn cmd-reducer [state cmd]
  (let [path-to-node (concat [:file-tree] (state :path))]
    (case (subs cmd 0 2)
      "ls" (assoc-in
            state
            path-to-node
            (process-ls
             (rest (str/split-lines cmd))
             (get-in state path-to-node)))

      "cd" (assoc
            state
            :path
            (change-dir (state :path) cmd))

      :else state)))

(defn file-size [node]
  (if (map? node)
    (reduce + (map file-size (vals node)))
    node))

(defn directory-sizes []
  (->> (str/split (slurp "resources/day7.txt") #"\$ ")
       (rest)
       (reduce
        cmd-reducer
        {:path [] :file-tree {:/ {}}})
       (#(% :file-tree))
       (tree-seq map? vals)
       (filter map?)
       (map file-size)))

(defn part1 [sizes]
  (->> sizes
       (filter #(> 100000 %))
       (reduce +)))

(defn part2 [sizes]
  (let [minimum-to-free (->> (apply max sizes)
                             (- 70000000) 
                             (- 30000000))]
    (->> sizes
         (filter #(> % minimum-to-free))
         (apply min))))

(defn -main []
  (let [sizes (directory-sizes)]
    {:part1 (part1 sizes) :part2 (part2 sizes)}))

(-main)