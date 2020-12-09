(require '[clojure.string :as str])

(def sample-input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")
(def real-input (slurp "day09/input"))

(defn valid? [[numbers n]]
  (let [combinations (for [a numbers
                           b numbers
                           :when (< a b)]
                       (+ a b))]
    (some #{n} combinations)))

(defn solve-part-1 [preamble-size input]
  (->> input
       str/split-lines
       (map read-string)
       (partition (inc preamble-size) 1)
       (map #(vector (butlast %) (last %)))
       (remove valid?)
       first))

(solve-part-1 5 sample-input)
(solve-part-1 25 real-input)

(defn search [numbers searched]
  (let [length (count numbers)]
    (for [n (range length)]
      (loop [previous (get numbers n)
             m (inc n)]
        (let [value (+ previous (get numbers m 0))]
          (cond
            (>= m length) :not-found
            (> value searched) :not-found
            (= value searched) [n m]
            :else (recur value (inc m))))))))

(defn solve-part-2 [input searched]
  (let [numbers (vec (map read-string (str/split-lines input)))
        [n m] (first (drop-while #{:not-found} (search numbers searched)))
        found-range (subvec numbers n (inc m))]
    (+ (reduce max found-range)
       (reduce min found-range))))

(solve-part-2 sample-input 127)
(solve-part-2 real-input 31161678)
