(defn split-range [[min max] direction]
  (let [middle (/ (+ min max) 2)]
    (case direction
      :low [min (int (Math/floor middle))]
      :up [(int (Math/ceil middle)) max]
      [min max])))

(def letter->direction
  {"F" :low
   "B" :up
   "L" :low
   "R" :up})

(defn find-in-range [range directions]
  (loop [range range
         directions directions]
    (if (empty? directions)
      (first range)
      (recur
       (split-range range (first directions))
       (rest directions)))))

(defn parse-line [line]
  (let [directions (map letter->direction
                        (clojure.string/split line #""))]
    [(take 7 directions) (drop 7 directions)]))

(find-in-range [0 127] (first (parse-line "FBFBBFFRLR")))
(find-in-range [0 8] (second (parse-line "FBFBBFFRLR")))

(defn unique-seat-id [[row column]]
  (+ (* row 8) column))

(defn solve-line [line]
  (let [[rows-dir columns-dir] (parse-line line)
        row (find-in-range [0 127] rows-dir)
        column (find-in-range [0 8] columns-dir)]
    (tap> [row column])
    (unique-seat-id [row column])))

(comment
  (add-tap println)
  (remove-tap println))

(solve-line "FBFBBFFRLR")
(solve-line "BFFFBBFRRR")
(solve-line "FFFBBBFRRR")
(solve-line "BBFFBBFRLL")

(def flight-ids (map solve-line (clojure.string/split-lines (slurp "day05/input"))))

(reduce max flight-ids)
;; 806

(->> flight-ids
     (sort >)
     (partition 2 1)
     (filter #(> (apply - %) 1))
     ffirst
     dec)
;; 562
