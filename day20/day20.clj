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

(defn find-tile-by-borders [matching-borders n]
  (->> (mapcat (juxt first third) matching-borders)
       frequencies
       (filter #(= n (second %)))
       (map first)))

(defn solve-part-1 [input]
  (let [matching-borders (find-matching-borders input)
        corners (find-tile-by-borders matching-borders 2) ]
    (apply * corners)))

(solve-part-1 sample-input)
(solve-part-1 real-input)

"
borders
  0
2   3
  1
reverse...
  4
6   7
  5"

(defn flip-horizontal [matrix]
  (into {}
        (map (fn [[[x y] v]] [[(Math/abs (- 9 x)) y] v]))
        matrix))

(defn flip-vertical [matrix]
  (into {}
        (map (fn [[[x y] v]] [[x (Math/abs (- 9 y))] v]))
        matrix))

(defn rot [[x y]] [(- 10 y 1) x])

(defn rotate [matrix]
  (into {}
        (map (fn [[coords v]] [(rot coords) v]))
        matrix))

(defn debug-layout [layout]
  (prn "--------------------------------------------------------------")
  (let [width (apply max (map first (keys layout)))
        height (apply max (map second (keys layout)))]
    (doseq [x (range (inc width))]
      (doseq [y (range (inc height))]
        (pr (layout [y x])))
      (prn))))

(comment
  (debug-layout (first (vals sample-input)))
  (debug-layout (flip-horizontal (first (vals sample-input))))
  (debug-layout (flip-vertical (first (vals sample-input))))
  (debug-layout (rotate (first (vals sample-input)))))

(let [matching-borders (find-matching-borders real-input)
      start (first (find-tile-by-borders matching-borders 2))
      tree {}
      neighbors (keep
                 (fn [[id idx id' idx' :as b]]
                   (or (when (= start id) id')
                       (when (= start id') id)))
                 matching-borders)]
  )
