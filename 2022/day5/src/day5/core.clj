(ns day5.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def input  (->> "input.txt"
                 slurp
                 str/split-lines
                 (split-with #(not (str/blank? %)))))

(defn parse-crates [crates]
  (->> crates
       (apply map str)
       (map str/trim)
       (filter #(re-matches #"\w+" %))
       vec))

(defn extract-digits [strings]
  (->> strings
       (filter #(re-matches #"\d+" %))
       (map #(Integer/parseInt %))))

(defn parse-instructions [instructions]
  (->>
   (rest instructions)
   (map #(str/split % #" "))
   (map extract-digits)))

(defn move-crates [crates [number from to]]
  (let [from-pile (nth crates (dec from))
        to-pile (nth crates (dec to))
        [moving-crates remaining-crates] (split-at number from-pile)
        moved-crates (apply str (reverse moving-crates))]
    (assoc crates
           (dec from) (apply str remaining-crates)
           (dec to) (str moved-crates to-pile))))

(defn follow-instructions [crates instructions]
  (if (empty? instructions)
    crates
    (follow-instructions (move-crates crates (first instructions))
                         (rest instructions))))

(defn part-1 [crates instructions]
  (->>
   (follow-instructions crates instructions)
   (map first)
   (apply str)))

(defn -main
  [& args]
  (let [[crates instructions] input
        parsed-crates (parse-crates crates)
        parsed-instructions (parse-instructions instructions)]

    (println "part 1" (part-1 parsed-crates parsed-instructions))))

