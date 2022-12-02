(ns day5.core
  (:gen-class)
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

(def input
  (->> "input.txt"
       slurp
       str/split-lines
       (mapcat #(str/split %  #" -> "))
       (mapcat #(str/split %  #","))
       (map #(Integer/parseInt %))
       (partition 4)
       (map #(partition 2 %))))

(defn build-map [x y]
  (vec (take y (repeatedly
                #(vec (take x (repeat 0)))))))

(defn draw-horizontal [y [x1 x2] sea-map]
  (loop [inital-x x1
         final-x x2
         working-map sea-map]
    (let [updated-map (update-in working-map [y inital-x] inc)]
      (if (= inital-x final-x)
        updated-map
        (recur (inc inital-x) x2 updated-map)))))

(defn draw-vertical [x [y1 y2] sea-map]
  (loop [inital-y y1
         final-y y2
         working-map sea-map]
    (let [updated-map (update-in working-map [inital-y x] inc)]
      (if (= inital-y final-y)
        updated-map
        (recur (inc inital-y) y2 updated-map)))))

(defn draw-diagonal [[[x1 y1] [x2 y2]] sea-map]
  (let [y-direction (if (< y1 y2) inc dec)]
    (loop [inital-x x1
           inital-y y1
           working-map sea-map]
      (let [updated-map (update-in working-map [inital-y inital-x] inc)]
        (if (= inital-x x2)
          updated-map
          (recur (inc inital-x) (y-direction inital-y) updated-map))))))

(defn direction [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    (partial draw-vertical x1 (sort [y1 y2]))
    (if (= y1 y2)
      (partial draw-horizontal y1 (sort [x1 x2]))
      (partial draw-diagonal (sort-by first [[x1 y1] [x2 y2]])))))

(defn mark-map [sea-map cordinates]
  (let [[cord-1 cord-2] (first cordinates)
        draw-function (direction cord-1 cord-2)]
    (if (seq cordinates)
      (recur (draw-function sea-map) (rest cordinates))
      sea-map)))

(defn part-1 [input]
  (let [sea-map (build-map 1000 1000)
        marked-map (mark-map sea-map input)]
    (doall (map println marked-map))
    (println (->> marked-map
                  flatten
                  (filter #(<= 2 %))
                  count))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (part-1 input))
