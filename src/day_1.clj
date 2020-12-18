(ns day_1)

(def input (slurp "resources/input_day_1.txt"))
(def example-1 '("1721" "979" "366" "299" "675" "1456"))

(def entries (map #(Integer/parseInt %) (clojure.string/split-lines input)))

(defn get-2020-multiples-part-1 [entries]
  (for [entries-1 entries
        entries-2 entries]
    (when (= (+ entries-1 entries-2)
             2020)
      (* entries-1 entries-2))))

;Part 1
(->> (get-2020-multiples-part-1 entries)
     (filter identity)
     distinct
     first)

(defn get-2020-multiples-part-2 [entries]
  (for [entries-1 entries
        entries-2 entries
        entries-3 entries]
    (when (= (+ entries-1 entries-2 entries-3)
             2020)
      (* entries-1 entries-2 entries-3))))

;Part 2
(->> (get-2020-multiples-part-2 entries)
     (filter identity)
     distinct
     first)
