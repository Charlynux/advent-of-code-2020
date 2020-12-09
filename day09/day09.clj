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
                           :when (not= a b)
                           :when (= (+ a b) n)]
                       :found)]
    (some? (first combinations))))

(defn solve-part-1 [preamble-size input]
  (->> input
       str/split-lines
       (map read-string)
       (partition (inc preamble-size) 1)
       (map #(vector (take preamble-size %) (last %)))
       (remove valid?)
       first))

(solve-part-1 5 sample-input)
(solve-part-1 25 real-input)
