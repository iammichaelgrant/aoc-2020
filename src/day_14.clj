(ns day_14)

(def input (clojure.string/split-lines (slurp "resources/input_day_14.txt")))
(def example-1 (clojure.string/split-lines "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"))

(defn parse-mask [mask-str]
  (->> (subs mask-str 7)
       reverse
       (map-indexed #(-> {%1 %2}))
       (apply merge)))

(defn parse-mem [mem-str]
  (let [split (clojure.string/split mem-str #"=")
        location (re-find #"\d+" (first split))
        value (re-find #"\d+" (second split))]
    {:location (Integer/parseInt location)
     :value (Integer/parseInt value)}))

(defn update-mem [memory mask mem-str]
  (let [parsed-mem (parse-mem mem-str)]
    (assoc memory (:location parsed-mem)
             (reduce (fn [mem-value mask-entry]
                       (if (= (val mask-entry) \1)
                         (bit-set mem-value (key mask-entry))
                         (bit-clear mem-value (key mask-entry))))
                     (:value parsed-mem)
                     mask))))

;Part 1
(let [program input]
  (loop [instructions program
         state {:mask {}
                :memory {}}]
    (if (empty? instructions)
      (->> state
           :memory
           vals
           (reduce +))
      (recur
        (rest instructions)
        (if (clojure.string/starts-with? (first instructions) "mask")
          (update state :mask (->> (first instructions)
                                   parse-mask
                                   (remove #(= \X %))
                                   constantly))
          (update state :memory #(update-mem % (:mask state) (first instructions))))))))

(def example-2 (clojure.string/split-lines "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"))

(defn update-mem-v2 [memory mask mem-str]
  (let [parsed-mem (parse-mem mem-str)
        addresses (reduce (fn [addresses mask-entry]
                            (case (val mask-entry)
                              \1 (map #(bit-set % (key mask-entry)) addresses)
                              \0 addresses
                              \X (concat
                                   (map #(bit-set % (key mask-entry)) addresses)
                                   (map #(bit-clear % (key mask-entry)) addresses))))
                          [(:location parsed-mem)]
                          mask)]
    (reduce (fn [memory address]
              (assoc memory address (:value parsed-mem)))
            memory
            addresses)))

;Part 2
(let [program input]
  (loop [instructions program
         state {:mask {}
                :memory {}}]
    (if (empty? instructions)
      (->> state
           :memory
           vals
           (reduce +))
      (recur
        (rest instructions)
        (if (clojure.string/starts-with? (first instructions) "mask")
          (update state :mask (->> (first instructions)
                                   parse-mask
                                   constantly))
          (update state :memory #(update-mem-v2 % (:mask state) (first instructions))))))))
