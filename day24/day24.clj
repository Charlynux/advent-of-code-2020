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

(->> (slurp "day24/input")
     clojure.string/split-lines
     (map #(move REFERENCE_TILE (parse-line %)))
     frequencies
     (filter (comp odd? second))
     count)
