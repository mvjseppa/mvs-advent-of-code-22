(ns aoc22.day7
  (:gen-class)
  (:require [clojure.string :as str]))

(defn change-dir [path cmd]
  (let [target-dir (subs (str/trim cmd) 3)]
    (println (str "cd: " path " + " target-dir))
    (if (= target-dir "..")
      (pop path)
      (conj path (keyword target-dir)))))

(defn get-ls-data [ls-row]
  (let [parts (str/split ls-row #" ")]
    {:path (keyword (parts 1)) 
     :payload (if (= (parts 0) "dir") {} (read-string (parts 0)))}))

(defn update-file-tree-node [ls-data node]
  (assoc node (ls-data :path) (ls-data :payload)))

(defn process-ls [ls-output node] 
  (loop [files ls-output node node]
    (if (empty? files)
      node
      (recur 
       (next files)
       (-> (first files)
           (get-ls-data)
           (update-file-tree-node node))))))

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

(defn -main []
  (->> (str/split (slurp "resources/day7-example.txt") #"\$ ")
       (rest)
       (reduce
        cmd-reducer 
        {:path [] :file-tree {:/ {}}})))

(-main)