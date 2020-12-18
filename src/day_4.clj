(ns day_4)

(def input (clojure.string/split-lines (slurp "resources/input_day_4.txt")))
(def example-1 (clojure.string/split-lines "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"))

(defn parse-fields [field]
  (let [parts (clojure.string/split field #":")]
    (hash-map (keyword (first parts)) (second parts))))

(defn parse-line [entry]
  (->> (clojure.string/split entry #" ")
       (map parse-fields)
       (apply merge)))

(defn parse-input [input]
  (reduce
    (fn [passports input-line]
      (if (empty? input-line)
        (conj passports {})
        (let [current-passport (last passports)
              new-fields (parse-line input-line)]
          (conj (pop passports)
                (merge current-passport new-fields)))))
    [{}]
    input))

(defn valid-passport? [passport]
  (every?
    #(contains? passport %)
    [:byr
     :iyr
     :eyr
     :hgt
     :hcl
     :ecl
     :pid]))

;Part 1
(->> input
     parse-input
     (map valid-passport?)
     (filter identity)
     count)

(defn valid-height? [height]
  (when (> (count height) 2)
    (let [str-length (count height)
          units (subs height (- str-length 2))
          value (subs height 0 (- str-length 2))]
      (or
        (and (= units "cm")
             (every? #(Character/isDigit %) value)
             (<= 150 (Integer/parseInt value) 193))
        (and (= units "in")
             (every? #(Character/isDigit %) value)
             (<= 59 (Integer/parseInt value) 76))))))

(defn valid-hair-color? [hair-color]
  (let [hash (first hair-color)
        characters (rest hair-color)]
    (and
      (= hash \#)
      (= (count characters) 6)
      (every?
        #(re-matches #"[a-f0-9]" (str %))
        characters))))

(defn valid-passport-2? [passport]
  (and
    (and (seq (:byr passport))
         (every? #(Character/isDigit %) (:byr passport))
         (<= 1920 (Integer/parseInt (:byr passport)) 2002))
    (and (seq (:iyr passport))
         (every? #(Character/isDigit %) (:iyr passport))
         (<= 2010 (Integer/parseInt (:iyr passport)) 2020))
    (and (seq (:eyr passport))
         (every? #(Character/isDigit %) (:eyr passport))
         (<= 2020 (Integer/parseInt (:eyr passport)) 2030))
    (valid-height? (:hgt passport))
    (valid-hair-color? (:hcl passport))
    (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} (:ecl passport))
    (and (= (count (:pid passport)) 9)
         (every? #(Character/isDigit %) (:pid passport))))
)

;Part 2
(->> input
     parse-input
     (map valid-passport-2?)
     (filter identity)
     count)
