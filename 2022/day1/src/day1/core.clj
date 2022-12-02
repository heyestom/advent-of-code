(ns day1.core
  (:require [clojure.string :as str]))

(def input (-> (slurp "./input.txt")
               str/split-lines))

(defn sum-backpacks [backpacks]
  (let [[current-backpack remaining-backpacks] (split-with #(not (str/blank? %)) backpacks)
        parsed-backpack (map #(Integer/parseInt %) current-backpack)
        current-total (reduce + parsed-backpack)]
    (if (empty? remaining-backpacks)
      [current-total]
      (conj
       (sum-backpacks (rest remaining-backpacks))
       current-total))))

(defn part-1
  "finds highest calorie count of all backpacks"
  [backpacks]
  (apply max (sum-backpacks backpacks)))

(defn part-2
  "sums the  calorie count of three highest calorie count backpacks"
  [backpacks]
  (reduce + (take 3 (sort > (sum-backpacks backpacks)))))

(defn -main
  "Finds the calorie total of backpack with the greatest calories"
  [& args]
  (println "part 1 total: " (part-1 input))
  (println "part 2 total: " (part-2 input)))
