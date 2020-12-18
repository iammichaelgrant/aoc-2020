(ns day_2)

(def input (slurp "resources/input_day_2.txt"))
(def example-1 '("1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc"))

(def entries (clojure.string/split-lines input))

(defn parse-entry [entry]
  (let [parts (clojure.string/split entry #" ")
        minmax (clojure.string/split (first parts) #"-")]
    {:min (Integer/parseInt (first minmax))
     :max (Integer/parseInt (second minmax))
     :letter (-> parts second first)
     :password (last parts)}))

(defn add-occurrences [entry]
  (assoc entry :occurrences (count (re-seq
                                     (re-pattern (str (:letter entry)))
                                     (:password entry)))))

(defn validate-entry [entry]
  (and (<= (:occurrences entry) (:max entry))
       (>= (:occurrences entry) (:min entry))))

;Part 1
(->> (map
       #(-> %
            parse-entry
            add-occurrences
            validate-entry)
       entries)
     (filter identity)
     count)

(defn parse-entry-2 [entry]
  (let [parts (clojure.string/split entry #" ")
        minmax (clojure.string/split (first parts) #"-")]
    {:pos-1 (Integer/parseInt (first minmax))
     :pos-2 (Integer/parseInt (second minmax))
     :letter (-> parts second first)
     :password (last parts)}))

(defn xor [exprs]
  (let [t-exprs (filter identity exprs)]
    (when (= (count t-exprs) 1)
      (first t-exprs))))

(defn validate-entry-2 [entry]
  (let [index-1 (dec (:pos-1 entry))
        index-2 (dec (:pos-2 entry))
        letter (:letter entry)
        password (:password entry)]
    (xor [(= (get password index-1) letter)
          (= (get password index-2) letter)])))

;Part 2
(->> (map
       #(-> %
            parse-entry-2
            validate-entry-2)
       entries)
     (filter identity)
     count)