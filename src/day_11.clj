(ns day_11)

(def input (clojure.string/split-lines (slurp "resources/input_day_11.txt")))
(def example-1 (clojure.string/split-lines "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"))

(defn xy [m x y]
  (when-let [row (when (contains? m y)
                   (nth m y))]
    (when (contains? row x)
      (nth row x))))

(defn occupied-s? [x]
  (= x \#))

(defn not-occupied-s? [x]
  (not (occupied-s? x)))

(defn count-occupied [m]
  (->> m
       (map #(->> %
                  (filter occupied-s?)
                  count))
       (reduce +)))

(defn adjacents [m x y]
  (for [xs (range (- x 1) (+ x 2))
        ys (range (- y 1) (+ y 2))
        :let [adj (xy m xs ys)]
        :when (and (not (and (= x xs)
                             (= y ys)))
                   adj)]
    adj))

(defn generate-new-value [m x y]
  (let [value (xy m x y)
        adjs (adjacents m x y)]
    (cond
      (and (= value \L)
           (every? not-occupied-s? adjs))
      \#
      (and (= value \#)
           (->> adjs
                (filter occupied-s?)
                count
                (<= 4)))
      \L
      :else
      value)))

(defn generate-new-state [state new-value-fn]
  (let [row-count (count state)
        col-count (count (first state))
        new-states (for [ys (range 0 row-count)
                         xs (range 0 col-count)]
                     (new-value-fn state xs ys))]
    (->> (partition col-count new-states)
         (map vec)
         vec)))

;Part 1
(loop [state input
       old-states #{state}
       iteration 1]
  (let [new-state (generate-new-state state generate-new-value)
        occupied (count-occupied new-state)]
    (if (contains? old-states new-state)
      occupied
      (recur
        new-state
        (conj old-states new-state)
        (inc iteration)))))

(defn visibles [m x y]
  (for [xs (range -1 2)
        ys (range -1 2)
        :let [vis (loop [current-x (+ x xs)
                         current-y (+ y ys)]
                    (if-not (= \. (xy m current-x current-y))
                      (xy m current-x current-y)
                      (recur (+ current-x xs)
                             (+ current-y ys))))]
        :when (and (or  (not= 0 xs)
                        (not= 0 ys))
                   vis)]
    vis))

(defn generate-new-value-2 [m x y]
  (let [value (xy m x y)
        visible-seats (visibles m x y)]
    (cond
      (and (= value \L)
           (every? not-occupied-s? visible-seats))
      \#
      (and (= value \#)
           (->> visible-seats
                (filter occupied-s?)
                count
                (<= 5)))
      \L
      :else
      value)))

;Part 2
(loop [state input
       old-states #{state}
       iteration 1]
  (let [new-state (generate-new-state state generate-new-value-2)
        occupied (count-occupied new-state)]
    (if (contains? old-states new-state)
      occupied
      (recur
        new-state
        (conj old-states new-state)
        (inc iteration)))))