(ns day6.core
  (:gen-class))

(def input (slurp "input.txt"))

(some (fn [chars] (when (apply = chars) chars)) (partition 4 1 "123456789999"))

(defn find-packet-index
  ([packet-size potential-packets]
   (find-packet-index packet-size potential-packets 0))
  ([packet-size potential-packets index]
   (if (= packet-size
          (->> potential-packets
               first
               distinct
               count))
     [index (first potential-packets)]
     (if (empty? potential-packets)
       "Failed"
       (recur
        packet-size
        (rest potential-packets)
        (inc index))))))

(defn part-1 [signal]
  (let [packet-size 4]
    (->> signal
         (partition packet-size 1)
         (find-packet-index packet-size)
         first
         (+ packet-size))))

(defn part-2 [signal]
  (let [packet-size 14]
    (->> signal
         (partition packet-size 1)
         (find-packet-index packet-size)
         first
         (+ packet-size))))

(defn -main
  [& args]
  (println "part-1:" (part-1 input))
  (println "part-2:" (part-2 input)))
