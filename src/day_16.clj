(ns day_16)

(def input (slurp "resources/input_day_16.txt"))
(def example-1 "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12")

(defn range-inclusive [start end]
  (range start (inc end)))

(defn parse-ints [strs]
  (map #(Integer/parseInt %) strs))

(defn parse-input [input]
  (let [splits (clojure.string/split input #"your ticket:")]
    {:rules   (->> (clojure.string/split-lines (first splits))
                   (map #(let [numbers (->> (re-seq #"\d+-\d+" %)
                                            (map (fn [rule-range]
                                                   (->> (clojure.string/split rule-range #"-")
                                                        parse-ints
                                                        (apply range-inclusive)
                                                        (apply hash-set))))
                                            (apply clojure.set/union))
                               field (->> (clojure.string/split % #":")
                                          first)]
                           {field numbers}))
                   (apply merge))
     :tickets (->> (clojure.string/split-lines (second splits))
                   (remove #(or (= "nearby tickets:" %)
                                (empty? %)))
                   (map #(->> (clojure.string/split % #",")
                              parse-ints)))}))

;Part 1
(let [{:keys [rules tickets]} (parse-input input)]
  (->> tickets
       rest
       (keep
         (fn [numbers]
           (keep
             (fn [number]
               (when (not-any? (fn [rule]
                                 (contains? rule number))
                               (vals rules))
                 number))
             numbers)))
       flatten
       (reduce +)))

(def example-2 "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9")

;Part 2
(let [{:keys [rules tickets]} (parse-input input)
      valid-tickets (filter (fn [ticket-numbers]
                              (every? (fn [number]
                                        (some (fn [rule]
                                                (contains? rule number))
                                              (vals rules)))
                                      ticket-numbers))
                            tickets)
      valid-fields (->> valid-tickets
                        (apply interleave)
                        (partition (count valid-tickets))
                        (map-indexed
                          (fn [idx position-numbers]
                            (->> (map
                                   (fn [rule]
                                     (when (every?
                                             (fn [number]
                                               (contains? (val rule) number))
                                             position-numbers)
                                       {(key rule) #{idx}}))
                                   rules)
                                 (apply merge))))
                        (apply merge-with clojure.set/union))
      field-positions (loop [candidate-fields valid-fields
                             selected-fields {}]
                        (if (empty? candidate-fields)
                          selected-fields
                          (let [selected-field (some (fn [field]
                                                       (when (= (count (val field)) 1)
                                                         field))
                                                     candidate-fields)]
                            (recur
                              (->> (dissoc candidate-fields (key selected-field))
                                   (map (fn [candidate-field]
                                          {(key candidate-field)
                                           (clojure.set/difference (val candidate-field)
                                                                   (val selected-field))}))
                                   (apply merge))
                              (assoc selected-fields
                                (key selected-field)
                                (first (val selected-field)))))))]
  (->> field-positions
       (filter (fn [field-position]
                 (clojure.string/starts-with?
                   (key field-position)
                   "departure")))
       (map (fn [position]
              (nth (first valid-tickets) (val position))))
       (reduce *)))
