(ns day3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "./input.txt")))

(defn find-miss-packed-item [backpack]
  (let [[compartment-1 compartment-2]
        (split-at (/ (count backpack) 2) backpack)]
    (->> compartment-2
         (mapcat #(map (fn [y] (when (= y %) %)) compartment-1))
         (drop-while nil?)
         first)))

(defn item-priority [item]
  (if (Character/isLowerCase item)
    (- (int item) 96)
    (- (int item) 38)))

(defn part-1 [backpacks]
  (->> backpacks
       (map find-miss-packed-item)
       (map item-priority)
       (reduce +)))

(defn -main
  [& args]
  (println "part-1 priority: " (part-1 input)))
