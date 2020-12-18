(ns day_3)

(def input (slurp "resources/input_day_3.txt"))
(def entries (clojure.string/split-lines input))

(def example-1 (clojure.string/split-lines "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"))

(defn add-repeats [entry]
  (lazy-seq
    (apply concat
           (repeat entry))))

(defn tree? [x]
  (= \# x))

(defn check-slope [x-inc y-inc]
  (loop [x 0
         y 0
         trees 0]
    (if-not (contains? entries y)
      trees
      (let [row (add-repeats (nth entries y))
            contents (nth row x)]
        (recur (+ x x-inc)
               (+ y y-inc)
               (+ trees (if (tree? contents) 1 0)))))))

;Part 1
(check-slope 3 1)

;Part 2
(->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
     (map #(apply check-slope %))
     (apply *))
