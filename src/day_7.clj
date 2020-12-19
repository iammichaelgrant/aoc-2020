(ns day_7)

(def input (clojure.string/split-lines (slurp "resources/input_day_7.txt")))
(def example-1 (clojure.string/split-lines "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."))

(defn parse-rule [rule]
  (let [parts (clojure.string/split rule #"contain|, ")
        stripped (map #(-> %
                           (clojure.string/replace #"bags?\.?" "")
                           clojure.string/trim
                           (clojure.string/replace #" " "-")
                           (clojure.string/split #"(?<=\d)-"))
                      parts)]
    (hash-map (keyword (ffirst stripped))
              (->> (rest stripped)
                   (map #(when-not (= (first %) "no-other")
                           (hash-map (keyword (second %))
                                     (Integer/parseInt (first %)))))
                   (filter identity)
                   (apply merge {})))))

(defn parse-rules [text]
  (->> text
       (map parse-rule)
       (apply merge)))

(defn may-contain? [rules current-rule bag-type]
  (cond
    (empty? (val current-rule))
    false

    (contains? (val current-rule) bag-type)
    true

    :else
    (some
      #(may-contain? rules (find rules (first %)) bag-type)
      (val current-rule))))

;Part 1
(->> input
     parse-rules
     ((fn [rules]
        (filter
          #(may-contain? rules % :shiny-gold)
          rules)))
     count)


(def example-2 (clojure.string/split-lines "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags."))

(defn count-inner [bag-type rules]
  (let [rules-for-bag (get rules bag-type)]
    (+ 1
       (if (empty? rules-for-bag)
         0
         (->> rules-for-bag
              (map #(* (val %)
                       (count-inner (key %) rules)))
              (reduce +))))))

;Part 2
(->> input
     parse-rules
     (count-inner :shiny-gold)
     dec)
