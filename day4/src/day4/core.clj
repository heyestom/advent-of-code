(ns day4.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def input (str/split-lines  (slurp "input.txt")))

(defn mark-board [target-number board]
  (map (fn [row] (map #(if (= target-number %) "X" %) row)) board))

(defn prd [x] (= "X" x))

(defn check-winning-board [board]
  (let [rows board
        columns (apply map vector board)
        checks (concat rows columns)
        complete-row (filter #(every? prd %) checks)]
    (seq complete-row)))

(defn play-bingo [numbers boards]
  (let [target-number (first numbers)
        updated-boards (map #(mark-board target-number %) boards)
        winning-boards (filter check-winning-board updated-boards)]
    (if (not (empty? winning-boards))
      [target-number winning-boards]
      (play-bingo (rest numbers) updated-boards))))

(defn score-board [board]
  (->> board
       (apply concat)
       (remove #(= "X" %))
       (map #(Integer/parseInt %))
       (reduce +)))


(score-board [["1" "X"]["2" "X"]])

(defn part-1 []
  (let [numbers (str/split (first input) #",")
        boards (partition 5 (remove str/blank?  (rest input)))
        vec-boards (map #(map (fn [row] (str/split (str/trim row) #"\s+")) %) boards)
        [final-number winning-boards] (play-bingo numbers vec-boards)
        winning-board (first winning-boards)]
    (println "part 1:")
    (println (str "winning board" (vec winning-board)))
    (println (str "final number" final-number))
    (println (str "board score "(score-board winning-board)))
    (println (str "Answer: " (* (Integer/parseInt final-number)
                                (score-board winning-board))))))


(defn play-bingo-badly [numbers boards]
  (let [target-number (first numbers)
        updated-boards (map #(mark-board target-number %) boards)
        losing-boards (remove check-winning-board updated-boards)]
    (if (= 1 (count losing-boards))
      (play-bingo (rest numbers) losing-boards)
      (play-bingo-badly (rest numbers) updated-boards))))

(defn part-2 []
  (let [numbers (str/split (first input) #",")
        boards (partition 5 (remove str/blank?  (rest input)))
        vec-boards (map #(map (fn [row] (str/split (str/trim row) #"\s+")) %) boards)
        [final-number losing-boards] (play-bingo-badly numbers vec-boards)
        losing-board (first losing-boards)]
    (println "part 1:")
    (println (str "losing board" (vec losing-board)))
    (println (str "final number" final-number))
    (println (str "board score "(score-board losing-board)))
    (println (str "Answer: " (* (Integer/parseInt final-number)
                                (score-board losing-board))))))

(defn -main
  [& args]
  (part-1)
  (part-2))
