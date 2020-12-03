(defn read-trees-positions [map]
  (let [read-map-line (fn [y line] (map-indexed (fn [x v] (when (= "#" v) [x y])) line))]
    (->> map
         (map-indexed read-map-line)
         (mapcat identity)
         (remove nil?)
         set)))

(defn parse-input [input]
  (let [map-lines (->> input
                       (clojure.string/split-lines)
                       (map #(clojure.string/split % #"")))
        width (count (first map-lines))
        height (count map-lines)]
    [width height (read-trees-positions map-lines)]))

(defn move [[x y] [offset-x offset-y] [width _ _]]
  (let [next-x (+ x offset-x)
        next-y (+ y offset-y)]
    [(mod next-x width) next-y]))

(def INITIAL_POSITION [0 0])
(def SLOPE [3 1])

(comment
  (def sample-input "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

  (def sample-map (parse-input sample-input))

  (take 10 (iterate #(move % SLOPE sample-map) INITIAL_POSITION))

  (def sample-positions
    (take-while #(< (second %) (second sample-map))
                (iterate #(move % SLOPE sample-map) INITIAL_POSITION)))

  (count (filter (comp some? (nth sample-map 2)) sample-positions)))


(defn solve-part-1 [input]
  (let [[width height trees] (parse-input input)
        end-not-reached? (fn [[x y]] (< y height))]
    (->> INITIAL_POSITION
         (iterate #(move % SLOPE [width]))
         (take-while end-not-reached?)
         (filter (comp some? trees))
         count)))
(comment
  (solve-part-1 sample-input))

(solve-part-1 (slurp "day03/input"))
;; 252
