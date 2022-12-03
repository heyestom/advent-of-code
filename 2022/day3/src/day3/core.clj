(ns day3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "./input.txt")))

(defn find-miss-packed-item [backpack]
  (let [[compartment-1 compartment-2]
        (split-at (/ (count backpack) 2) backpack)]
    (->> compartment-2
         (mapcat #(map (fn [y] (when (= y %) %)) compartment-1))
         (remove nil?)
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

(defn find-common-item [backpacks]
  (let [[a b c] backpacks]
    (->>
     (mapcat
      (fn [x]
        (mapcat
         (fn [y]
           (map
            (fn [z] (when (= x y z) z))
            c))
         b))
      a)
     (remove nil?)
     (first))))



(defn part-2 [backpacks]
  (->> backpacks
       (partition 3)
       (map find-common-item)
       (map item-priority)
       (reduce +)))

(defn -main
  [& args]
  (println "part-1 priority: " (part-1 input))
  (println "part-2 priority: " (part-2 input)))
