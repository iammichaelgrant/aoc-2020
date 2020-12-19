(ns day_6)

(def input (slurp "resources/input_day_6.txt"))
(def example-1 "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

;Part 1
(->> (clojure.string/split input #"\n\n")
     (map #(-> %
               (clojure.string/replace "\n" "")
               distinct
               count))
     (reduce +))

;Part 2
(->> (clojure.string/split input #"\n\n")
     (map #(->> %
                (clojure.string/split-lines)
                (map set)
                (apply clojure.set/intersection)
                count))
     (reduce +))
