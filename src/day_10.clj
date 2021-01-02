(ns day_10)

(def input (clojure.string/split-lines (slurp "resources/input_day_10.txt")))
(def example-1 (clojure.string/split-lines "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"))
(def example-2 (clojure.string/split-lines "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"))

;Part 1
(loop [adapters (->> input
                     (map #(Integer/parseInt %))
                     (#(conj % 0 (+ 3 (apply max %))))
                     sort)
       differences {:1 0 :2 0 :3 0}]
  (if-not (next adapters)
    (* (:1 differences) (:3 differences))
    (recur (next adapters)
           (update differences
                   (keyword (str (- (second adapters)
                                    (first adapters))))
                   inc))))

;Part 2
(let [adapters (->> input
                    (map #(Integer/parseInt %))
                    (#(conj % 0))
                    sort)
      branch-mapping (for [adapter-x adapters
                           adapter-y adapters
                           :when (<= 1 (- adapter-y adapter-x) 3)]
                       (sorted-map adapter-x [adapter-y]))]
  (->> branch-mapping
       (apply (partial merge-with concat))
       (map #(count (val %)))
       (partition-by #(= 1 %))
       (map #(case %
               [3 3 2] 7
               [3 2] 4
               [2] 2
               1))
       (reduce *)))
