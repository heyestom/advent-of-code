(ns day2.core
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "./input.txt")))

(defn part-1
  " Scores rock paper scissors games:
  A Rock
  B Paper
  C Scissors
  X Rock
  Y Paper
  Z Scissors
  "
  [rock-paper-scissors-rounds]
  (let [round-input-to-score
        {"A X" 4 ;; 1 + 3 draw 
         "A Y" 8 ;; 2 + 6 win
         "A Z" 3 ;; 3 + 0 loss
         "B X" 1 ;; 1 + 0 loss
         "B Y" 5 ;; 2 + 3 draw
         "B Z" 9 ;; 3 + 6 win
         "C X" 7 ;; 1 + 6 win
         "C Y" 2 ;; 2 + 0 loss
         "C Z" 6 ;; 3 + 3 draw
         }]
    (reduce + (map round-input-to-score
                   rock-paper-scissors-rounds))))

(defn part-2
  "Scores rock paper scissors games:
  A Rock
  B Paper
  C Scissors
  X Lose
  Y Draw
  Z Win
  "
  [rock-paper-scissors-rounds]
  (let [round-input-to-score
        {"A X" 3 ;; scissors 3 + 0 loss
         "A Y" 4 ;; rock 1 + draw 3
         "A Z" 8 ;; paper 2 + 6 win
         "B X" 1 ;; rock 1 + 0 loss
         "B Y" 5 ;; paper 2 + draw 3
         "B Z" 9 ;; scissors 3 + 6 win
         "C X" 2 ;; paper 2 + 0 loss
         "C Y" 6 ;; scissors 3 + draw 3
         "C Z" 7 ;; rock 1 + 6 win
         }]
    (reduce + (map round-input-to-score
                   rock-paper-scissors-rounds))))

(defn -main
  [& args]
  (println "Part 1 score: " (part-1 input))
  (println "Part 2 score: " (part-2 input)))
