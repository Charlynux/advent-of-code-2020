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

(def initial-layout (create-matrix (slurp "day24/input")))

(def black-tiles (filter (comp #{1} second) initial-layout))

;; Part 1
(count black-tiles)
