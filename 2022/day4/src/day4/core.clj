(ns day4.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn parse-coordinates [coords]
  (map #(Integer/parseInt %) coords))

(def input
  (->> "input.txt"
       slurp
       str/split-lines
       (map #(->> (str/split % #"[-|,]")
                  (parse-coordinates)))))

(defn assignments-fully-contained?
  [[elf-a-start elf-a-end elf-b-start elf-b-end]]
  (or
   (and
    (<= elf-a-start elf-b-start)
    (>= elf-a-end elf-b-end))
   (and
    (<= elf-b-start elf-a-start)
    (>= elf-b-end elf-a-end))))

(defn part-1 [assignments]
  (->> assignments
       (map assignments-fully-contained?)
       (filter true?)
       count))

(defn assignments-overlap?
  "Is the end of the range with the smallest
  start coordinate greater than or equal to
  the start of the range with the largest
  starting coordinate?"
  [[elf-a-start elf-a-end elf-b-start elf-b-end]]
  (if (< elf-a-start elf-b-start)
    (>= elf-a-end elf-b-start)
    (>= elf-b-end elf-a-start)))

(defn part-2 [assignments]
  (->> assignments
       (map assignments-overlap?)
       (filter true?)
       count))

(defn -main
  [& args]
  (println "Part 1 - assignments fully contained:" (part-1 input))
  (println "Part 2 - assignments overlapping:" (part-2 input)))
