(require '[cognitect.transcriptor :refer (check!)])

(defn parse-line [line]
  (let [[min max letter password]
        (rest (re-find #"(\d+)\-(\d+) ([a-z]): (\w+)" line))]
    [[(Integer/parseInt min) (Integer/parseInt max) (first letter)] password]))

(parse-line "1-3 a: abcde")
(check! #{[[1 3 \a] "abcde"]})

(parse-line "1-3 b: cdefg")
(check! #{[[1 3 \b] "cdefg"]})

(parse-line "2-9 c: ccccccccc")
(check! #{[[2 9 \c] "ccccccccc"]})

(def example "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(defn respect-policy? [[[min max letter] password]]
  (let [nb (get (frequencies password) letter 0)]
    (<= min nb max)))

(respect-policy? [[1 3 \a] "abcde"])
(respect-policy? [[1 3 \b] "cdefg"])
(respect-policy? [[2 9 \c] "ccccccccc"])

(defn solve-part-1 [input]
  (->> input
       (clojure.string/split-lines)
       (map parse-line)
       (filter respect-policy?)
       count))

(solve-part-1 example)
(check! #{2})

(solve-part-1 (slurp "day02/input"))
;; 416

(defn respect-official-policy? [[[pos1 pos2 letter] password]]
  (let [letter1 (get password (dec pos1))
        letter2 (get password (dec pos2))]
    (and (or (= letter letter1) (= letter letter2))
         (not= letter letter1 letter2))))

(respect-official-policy? [[1 3 \a] "abcde"])
(respect-official-policy? [[1 3 \b] "cdefg"])
(respect-official-policy? [[2 9 \c] "ccccccccc"])

(defn solve-part-2 [input]
  (->> input
       (clojure.string/split-lines)
       (map parse-line)
       (filter respect-official-policy?)
       count))

(solve-part-2 example)
(check! #{1})

(solve-part-2 (slurp "day02/input"))
