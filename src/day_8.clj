(ns day_8)

(def input (clojure.string/split-lines (slurp "resources/input_day_8.txt")))
(def example-1 (clojure.string/split-lines "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"))

(defn parse-instruction [text]
  (-> text
      (clojure.string/split #" |(?<=[+-])")
      (#(hash-map :op (keyword (first %))
                  :value (if (= (second %) "+")
                           (Integer/parseInt (nth % 2))
                           (- (Integer/parseInt (nth % 2))))))))

;Part 1
(let [instructions (map parse-instruction input)]
  (loop [acc 0
         current-instruction 0
         instructions-visited #{}]
    (if (contains? instructions-visited current-instruction)
      acc
      (let [instruction (nth instructions current-instruction)
            op (:op instruction)
            value (:value instruction)]
        (recur
          (case op
            :nop acc
            :acc (+ acc value)
            :jmp acc)
          (case op
            :nop (+ 1 current-instruction)
            :acc (+ 1 current-instruction)
            :jmp (+ current-instruction value))
          (conj instructions-visited current-instruction))))))


;Part 2
(defn run-program [instructions]
  (loop [acc 0
         current-instruction 0
         instructions-visited #{}]
    (cond
      (>= current-instruction (count instructions))
      acc
      (contains? instructions-visited current-instruction)
      nil
      :else
      (let [instruction (nth instructions current-instruction)
            op (:op instruction)
            value (:value instruction)]
        (recur
          (case op
            :nop acc
            :acc (+ acc value)
            :jmp acc)
          (case op
            :nop (+ 1 current-instruction)
            :acc (+ 1 current-instruction)
            :jmp (+ current-instruction value))
          (conj instructions-visited current-instruction))))))

(->> (mapv parse-instruction input)
     (#(repeat (count %) %))
     (map-indexed
       (fn [idx instructions]
         (let [current-op (:op (nth instructions idx))]
           (update-in
             instructions
             [idx :op]
             (constantly
               (case current-op
                 :nop :jmp
                 :jmp :nop
                 current-op))))))
     (keep run-program)
     first)
