(require '[clojure.string :as str])

(def dir->move
  {"e"  [1 0]
   "se" [1 -1]
   "sw" [0 -1]
   "w"  [-1 0]
   "nw" [-1 1]
   "ne" [0 1]})

(def sample-input "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(defn move [start moves]
  (reduce
   (fn [[x y] dir]
     (mapv + [x y] (dir->move dir)))
   start moves))

(defn parse-line [line]
  (re-seq #"e|se|sw|w|nw|ne" line))

(def REFERENCE_TILE [0 0])

(defn parse-input [input]
  (map parse-line (str/split-lines input)))

(defn flip [value] (if (= value 1) 0 1))

(defn flip-tile [matrix tile]
  (update matrix tile flip))

(defn create-matrix [input]
  (reduce flip-tile {}
          (map #(move REFERENCE_TILE %)
               (parse-input input))))

(defn keep-black-tiles [matrix]
  (into #{} (keep #(when (= 1 (second %)) (first %)) matrix)))


(defn find-initial-layout [input]
  (keep-black-tiles (create-matrix input)))

;; Part 1
(count (find-initial-layout (slurp "day24/input")))

;; "Game of Life Rules"
;; - Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
;; - Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.

(defn neighbors [coord]
  (map #(mapv + % coord) (vals dir->move)))

(defn generation [black-tiles]
  (let [nb-neighbors (frequencies (mapcat neighbors black-tiles))]
    (into #{}
          (keep (fn [[coord nb]] (cond
                                   (and (black-tiles coord)
                                        (<= 1 nb 2)) coord
                                   (and (not (black-tiles coord))
                                        (= 2 nb)) coord)))
          nb-neighbors)))

(defn solve-part-2 [input]
  (count (nth (iterate generation (find-initial-layout input)) 100)))

(solve-part-2 sample-input)
(solve-part-2 (slurp "day24/input"))
