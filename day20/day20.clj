(require '[clojure.string :as str])

(defn parse-tile [tile]
  (let [[description & matrix] (str/split-lines tile)
        id (read-string (second (re-find #"Tile (\d+):" description)))
        matrix-map (into {}
                         (comp
                          (map-indexed
                           (fn [y row]
                             (map-indexed (fn [x value]
                                            [[x y] value]) row)))
                          cat)
                         matrix)]
    [id matrix-map]))

(parse-tile
 "Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")

(defn parse-input [input]
  (into {}
        (map
         parse-tile
         (str/split input #"\R\R"))))

(def borders-pos
  (let [borders (concat
                 (for [y [0 9]]
                   (for [x (range 0 10)]
                     [x y]))
                 (for [x [0 9]]
                   (for [y (range 0 10)]
                     [x y])))]
    (concat
     borders
     (map reverse borders))))

(defn find-borders [matrix]
  (for [border borders-pos]
    (mapv matrix border)))

(def sample-input (parse-input (slurp "day20/sample-input")))
(def real-input (parse-input (slurp "day20/input")))

(defn find-matching-borders [input]
  (let [tiles-borders (map (fn [[id matrix]] [id (find-borders matrix)]) input)]
   (for [[id borders] tiles-borders
         [id' borders'] tiles-borders
         :when (< id id')
         ;; Don't compare reverse borders together
         border (take 4 borders)
         border' borders'
         :when (= border border')]
      [id (.indexOf borders border)
       id' (.indexOf borders' border')])))

(defn third [xs] (nth xs 2))

(defn solve-part-1 [input]
  (let [matching-borders (find-matching-borders input)
        tiles (mapcat (juxt first third) matching-borders)]
    (->> tiles
         frequencies
         (filter #(= 2 (second %)))
         (map first)
         (apply *))))

(solve-part-1 sample-input)
(solve-part-1 real-input)
