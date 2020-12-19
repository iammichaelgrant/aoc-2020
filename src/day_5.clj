(ns day_5)

(def input (clojure.string/split-lines (slurp "resources/input_day_5.txt")))
(def example-1 "FBFBBFFRLR")

(defn power [n exp]
     (apply * (repeat exp n)))

(defn calc-row [sections low-char]
     (loop [acc 0
            sections sections]
          (if (empty? sections)
               acc
               (recur (+ acc
                         (if (= (first sections)
                                low-char)
                              0
                              (power 2 (dec (count sections)))))
                      (rest sections)))))

(defn seat-number [seat-code]
     (let [rows (subs seat-code 0 7)
           cols (subs seat-code 7 10)
           row  (calc-row rows \F)
           col (calc-row cols \L)]
          (+ (* row 8)
             col)))

;Part 1
(->> input
     (map seat-number)
     (apply max))

(defn find-missing-seat [seats]
     (loop [seats seats] (if (= (+ (first seats) 1)
                                (second seats))
                              (recur (rest seats))
                              (+ (first seats)
                                 1))))

;Part 2
; 1
(->> input
     (map seat-number)
     sort
     find-missing-seat)
