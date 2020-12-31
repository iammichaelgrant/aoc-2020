(ns day_9)

(def input (clojure.string/split-lines (slurp "resources/input_day_9.txt")))
(def example-1 (clojure.string/split-lines "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576"))

(defn contains-sum? [nums n]
  (some #{n} (for [a nums
                   b nums
                   :when (< a b)]
               (+ a b))))

;Part 1
(let [preamble-length 25
      numbers (map #(Integer/parseInt %) input)]
  (loop [current-number preamble-length]
    (let [current-val (nth numbers current-number)
          previous-vals (take preamble-length (nthnext
                                                numbers
                                                (- current-number preamble-length)))]
      (if-not (contains-sum? previous-vals current-val)
        current-val
        (recur (inc current-number))))))

;Part 2
(loop [numbers (map #(Integer/parseInt %) input)
       current-numbers (take 1 numbers)
       next-numbers (rest numbers)]
  (let [current-total (apply + current-numbers)
        invalid-number 400480901]
    (cond
      (= current-total invalid-number)
      (+ (apply min current-numbers)
         (apply max current-numbers))

      (empty? numbers)
      :not-found

      :else
      (recur (if (< current-total invalid-number)
               numbers
               (rest numbers))
             (if (< current-total invalid-number)
               (conj current-numbers (first next-numbers))
               (take 1 numbers))
             (if (< current-total invalid-number)
               (rest next-numbers)
               (rest numbers))))))