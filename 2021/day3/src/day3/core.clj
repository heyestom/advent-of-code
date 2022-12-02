(ns day3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def input (->
            (slurp "./input.txt")
            (str/split-lines)))

(def iinc (fnil inc 0))

(defn- keyword-for-bit [bit]
  (if (= \0 bit)
    :zero
    :one))

(defn bit-frequencies [acc bin]
  (into {}
        (map-indexed
         (fn [index bit]
           {index (update (get acc index) (keyword-for-bit bit) iinc)}) bin)))

(defn- gamma-digits [val] (if (< (get val :zero 0) (get val :one 0))
                            "1"
                            "0"))

(defn- epsilon-digits [val] (if (> (get val :zero 0) (get val :one 0))
                              "1"
                              "0"))

(defn part-1 []
  (let [bit-frequencies-by-index (reduce bit-frequencies {} input)
        gamma-string (->> bit-frequencies-by-index
                          (sort-by key)
                          vals
                          (map gamma-digits)
                          (str/join))
        epsilon-string (->> bit-frequencies-by-index
                            (sort-by key)
                            vals
                            (map epsilon-digits)
                            (str/join))]
    (println (str "g bin" gamma-string))
    (println (str "g dec" (Integer/parseInt gamma-string 2)))
    (println (str "e bin" epsilon-string))
    (println (str "e dec" (Integer/parseInt epsilon-string 2)))
    (str "Answer: " (* (Integer/parseInt gamma-string 2) (Integer/parseInt epsilon-string 2)))))

(defn oxygen-rating  [input index]
  (let [bit-frequencies-by-index (->> (reduce bit-frequencies {} input)
                                      (sort-by key)
                                      vals)
        most-frequant-at-target (nth bit-frequencies-by-index index)
        target-bit (if (> (get most-frequant-at-target :zero 0)
                          (get most-frequant-at-target :one 0))
                     \0
                     \1)
        filtered-input (filter (fn [string] (= target-bit (nth string index))) input)]
    (if (<= (count filtered-input) 1)
      (first filtered-input)
      (oxygen-rating filtered-input (inc index)))))

(defn co2-rating  [input index]
  (let [bit-frequencies-by-index (->> (reduce bit-frequencies {} input)
                                      (sort-by key)
                                      vals)
        most-frequant-at-target (nth bit-frequencies-by-index index)
        target-bit (if (<= (get most-frequant-at-target :zero 0)
                           (get most-frequant-at-target :one 0))
                     \0
                     \1)
        filtered-input (filter (fn [string] (= target-bit (nth string index))) input)]
    (if (<= (count filtered-input) 1)
      (first filtered-input)
      (co2-rating filtered-input (inc index)))))

(defn part-2 []
  (let [oxygen (oxygen-rating input 0)
        co2 (co2-rating input 0)]
    (println (str "O2 bin " oxygen))
    (println (str "CO2 bin " co2))
    (println (str "O2 dec " (Integer/parseInt oxygen 2)))
    (println (str "CO2 dec " (Integer/parseInt co2 2)))
    (str "Answer: " (* (Integer/parseInt oxygen 2) (Integer/parseInt co2 2)))))

(defn -main [& args]
  (println (str "part 1: " (part-1)))
  (println "-------------------------")
  (println (str "part 2: " (part-2))))
