(ns day6.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def input
  (->>
   (str/split (slurp "input.txt") #",")
   (map #(str/trim %))
   (map #(Integer/parseInt %))))

(defn update-fish [fish]
  (if (= 0 fish)
    6
    (dec fish)))

(defn part-1 [input days]
  (let [new-fish-count (count (filter zero? input))
        new-fish (take new-fish-count (repeat 8))
        updated-fish (concat (map update-fish input) new-fish)]
    (if (zero? (dec days))
      updated-fish
      (recur updated-fish (dec days)))))

(defn project-fish-growth [fish-groups day]
  (let [reproducing-fish (first fish-groups)
        rest-fish (-> fish-groups
                      rest
                      vec
                      (update 6 + reproducing-fish))]
    (if (zero? day)
      fish-groups
      (project-fish-growth (concat rest-fish [reproducing-fish])
                           (dec day)))))

(defn part-2 [input days]
  (let [fish-counts (->> input
                         (concat '(0 1 2 3 4 5 6 7 8))
                         (group-by identity)
                         (sort-by key)
                         (map val)
                         (map count)
                         (map dec))]
    (reduce +
            (project-fish-growth fish-counts days))))

;; (part-2 [3 1 1 1 1 1 3 3 3 4 4 3 3 5 2])

(defn -main
  "I project fish growth"
  [& args]
  (println (str "Total fish part 1: " (count (part-1 input 80))))
  (println (str "Total fish part 2: " (part-2 input 256))))
