(ns day_17)

(def input (slurp "resources/input_day_17.txt"))
(def example-1 ".#.\n..#\n###")

(defn parse-input [inp]
  (->> (clojure.string/split-lines inp)
       (mapv #(-> % char-array seq vec))
       vector))

(defn range-inclusive [start end]
  (range start (inc end)))

(defn active? [x]
  (= x \#))

(defn inactive? [x]
  (not (= x \#)))

(defn cube-value
  ([pocket-dimension cube]
   (get-in pocket-dimension (reverse cube) \.))
  ([pocket-dimension x y z]
   (get-in pocket-dimension [z y x] \.)))

(defn set-xyz
  ([pocket-dimension cube value]
   (assoc-in pocket-dimension (reverse cube) value))
  ([pocket-dimension x y z value]
   (assoc-in pocket-dimension [z y x] value)))

(defn neighbours
  ([pocket-dimension cube]
   (apply neighbours pocket-dimension cube))
  ([pocket-dimension x y z]
   (for [xs (range (- x 1) (+ x 2))
         ys (range (- y 1) (+ y 2))
         zs (range (- z 1) (+ z 2))
         :let [adj (cube-value pocket-dimension xs ys zs)]
         :when (and (not (and (= x xs)
                              (= y ys)
                              (= z zs))))]
     adj)))

(defn total-active [pocket-dimension active-planes]
  (->> (for [xs (:x active-planes)
             ys (:y active-planes)
             zs (:z active-planes)
             :let [state (cube-value pocket-dimension xs ys zs)]]
         state)
       (filter active?)
       count))

;Part 1
(loop [iterations 0
       pocket-dimension (parse-input input)
       active-planes {:x (range 0 (count (ffirst pocket-dimension)))
                      :y (range 0 (count (first pocket-dimension)))
                      :z (range 0 (count pocket-dimension))}]
  (if (= iterations 6)
    (total-active pocket-dimension active-planes)
    (let [new-active-planes {:x (range-inclusive (-> active-planes :x first dec)
                                                 (-> active-planes :x last inc))
                             :y (range-inclusive (-> active-planes :y first dec)
                                                 (-> active-planes :y last inc))
                             :z (range-inclusive (-> active-planes :z first dec)
                                                 (-> active-planes :z last inc))}]
      (recur
        (inc iterations)
        (let [candidate-cubes (for [xs (:x new-active-planes)
                                    ys (:y new-active-planes)
                                    zs (:z new-active-planes)]
                                [xs ys zs])]
          (reduce (fn [new-pocket-dimension cube]
                    (set-xyz new-pocket-dimension
                             cube
                             (cond (and (active? (cube-value pocket-dimension cube))
                                        (->> (neighbours pocket-dimension cube)
                                             (filter active?)
                                             count
                                             (#(<= 2 % 3))))
                                   \#
                                   (and (inactive? (cube-value pocket-dimension cube))
                                        (->> (neighbours pocket-dimension cube)
                                             (filter active?)
                                             count
                                             (#(= % 3))))
                                   \#
                                   :else
                                   \.)))
                  {}
                  candidate-cubes))
        new-active-planes))))

(defn xyzw
  ([pocket-dimension hcube]
   (get-in pocket-dimension (reverse hcube) \.))
  ([pocket-dimension x y z w]
   (get-in pocket-dimension [w z y x] \.)))

(defn set-xyzw
  ([pocket-dimension hcube value]
   (assoc-in pocket-dimension (reverse hcube) value))
  ([pocket-dimension x y z w value]
   (assoc-in pocket-dimension [w z y x] value)))

(defn neighbours-hcube
  ([pocket-dimension hcube]
   (apply neighbours-hcube pocket-dimension hcube))
  ([pocket-dimension x y z w]
   (for [xs (range (- x 1) (+ x 2))
         ys (range (- y 1) (+ y 2))
         zs (range (- z 1) (+ z 2))
         ws (range (- w 1) (+ w 2))
         :let [adj (xyzw pocket-dimension xs ys zs ws)]
         :when (and (not (and (= x xs)
                              (= y ys)
                              (= z zs)
                              (= w ws))))]
     adj)))

(defn total-active-hcube [pocket-dimension active-planes]
  (->> (for [xs (:x active-planes)
             ys (:y active-planes)
             zs (:z active-planes)
             ws (:w active-planes)
             :let [state (xyzw pocket-dimension xs ys zs ws)]]
         state)
       (filter active?)
       count))

;Part 2
(loop [iterations 0
       pocket-dimension (vector (parse-input input))
       active-planes {:x (range 0 (count (first (ffirst pocket-dimension))))
                      :y (range 0 (count (ffirst pocket-dimension)))
                      :z (range 0 (count pocket-dimension))
                      :w (range 0 (count pocket-dimension))}]
  (if (= iterations 6)
    (total-active-hcube pocket-dimension active-planes)
    (let [new-active-planes {:x (range-inclusive (-> active-planes :x first dec)
                                                 (-> active-planes :x last inc))
                             :y (range-inclusive (-> active-planes :y first dec)
                                                 (-> active-planes :y last inc))
                             :z (range-inclusive (-> active-planes :z first dec)
                                                 (-> active-planes :z last inc))
                             :w (range-inclusive (-> active-planes :w first dec)
                                                 (-> active-planes :w last inc))}]
      (recur
        (inc iterations)
        (let [candidate-cubes (for [xs (:x new-active-planes)
                                    ys (:y new-active-planes)
                                    zs (:z new-active-planes)
                                    ws (:w new-active-planes)]
                                [xs ys zs ws])]
          (reduce (fn [new-pocket-dimension hcube]
                    (set-xyzw new-pocket-dimension
                              hcube
                              (cond (and (active? (xyzw pocket-dimension hcube))
                                         (->> (neighbours-hcube pocket-dimension hcube)
                                              (filter active?)
                                              count
                                              (#(<= 2 % 3))))
                                    \#
                                    (and (inactive? (xyzw pocket-dimension hcube))
                                         (->> (neighbours-hcube pocket-dimension hcube)
                                              (filter active?)
                                              count
                                              (#(= % 3))))
                                    \#
                                    :else
                                    \.)))
                  {}
                  candidate-cubes))
        new-active-planes))))
