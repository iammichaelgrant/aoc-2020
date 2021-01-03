(ns day_12)

(def input (clojure.string/split-lines (slurp "resources/input_day_12.txt")))
(def example-1 (clojure.string/split-lines "F10\nN3\nF7\nR90\nF11"))

;Part 1
(loop [instructions input
       position {:x 0 :y 0 :facing 90}]
  (if (empty? instructions)
    (+ (Math/abs (:x position))
       (Math/abs (:y position)))
    (let [instruction (first instructions)
          action (first instruction)
          value (Integer/parseInt (subs instruction 1))]
      (recur (rest instructions)
             (case action
               \N (update position :x #(+ % value))
               \S (update position :x #(- % value))
               \E (update position :y #(+ % value))
               \W (update position :y #(- % value))
               \L (update position :facing #(- % value))
               \R (update position :facing #(+ % value))
               \F (case (mod (:facing position) 360)
                    0 (update position :x #(+ % value))
                    90 (update position :y #(+ % value))
                    180 (update position :x #(- % value))
                    270 (update position :y #(- % value))))))))

(defn rotate-way [pos dir degrees]
  (prn pos dir degrees)
  (cond
    (= degrees 180)
    {:x (- (:x pos))
     :y (- (:y pos))}

    (or (and (= degrees 90)
             (= dir \R))
        (and (= degrees 270)
             (= dir \L)))
    {:x (:y pos)
     :y (- (:x pos))}

    (or (and (= degrees 90)
             (= dir \L))
        (and (= degrees 270)
             (= dir \R)))
    {:x (- (:y pos))
     :y (:x pos)}))

;Part 2
(loop [instructions input
       position {:ship {:x 0 :y 0}
                 :way {:x 10 :y 1}}]
  (if (empty? instructions)
    (+ (Math/abs (-> position :ship :x))
       (Math/abs (-> position :ship :y)))
    (let [instruction (first instructions)
          action (first instruction)
          value (Integer/parseInt (subs instruction 1))]
      (recur (rest instructions)
             (case action
               \N (update-in position [:way :y] #(+ % value))
               \S (update-in position [:way :y] #(- % value))
               \E (update-in position [:way :x] #(+ % value))
               \W (update-in position [:way :x] #(- % value))
               \L (update position :way #(rotate-way % \L value))
               \R (update position :way #(rotate-way % \R value))
               \F (update position :ship #(-> {:x (+ (:x %)
                                                     (* value
                                                        (-> position :way :x)))
                                               :y (+ (:y %)
                                                     (* value
                                                        (-> position :way :y)))})))))))