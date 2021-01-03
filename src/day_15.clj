(ns day_15)

(def input (map #(Integer/parseInt %) (clojure.string/split "6,19,0,5,7,13,1" #",")))
(def example-1 (map #(Integer/parseInt %) (clojure.string/split "0,3,6" #",")))

;Part 1
(loop [starting-numbers input
       turn 0
       last-spoken-map {}
       last-number nil]
  (if (= turn 2020)
    last-number
    (recur
      (rest starting-numbers)
      (inc turn)
      (assoc last-spoken-map last-number turn)
      (or (first starting-numbers)
          (when-let [last-spoken (get last-spoken-map last-number)]
            (- turn last-spoken))
          0))))

;Part 2
(loop [starting-numbers input
       turn 0
       last-spoken-map {}
       last-number nil]
  (if (= turn 30000000)
    last-number
    (recur
      (rest starting-numbers)
      (inc turn)
      (assoc last-spoken-map last-number turn)
      (or (first starting-numbers)
          (when-let [last-spoken (get last-spoken-map last-number)]
            (- turn last-spoken))
          0))))
