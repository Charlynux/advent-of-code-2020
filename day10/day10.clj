(def sample-input [16
                   10
                   15
                   5
                   1
                   11
                   7
                   19
                   6
                   12
                   4])
(def sample-input2 [28
                    33
                    18
                    42
                    31
                    14
                    46
                    20
                    48
                    47
                    24
                    23
                    49
                    45
                    19
                    38
                    39
                    11
                    1
                    32
                    25
                    35
                    8
                    17
                    7
                    9
                    4
                    2
                    34
                    10
                    3])

(def real-input (map read-string (clojure.string/split-lines (slurp "day10/input"))))

(defn solve-part-1 [numbers]
  (let [highest (reduce max numbers)]
    (->> (conj numbers 0 (+ highest 3))
         (sort >)
         (partition 2 1)
         (map #(apply - %))
         frequencies
         vals
         (apply *))))

(solve-part-1 sample-input)
(solve-part-1 sample-input2)
(solve-part-1 real-input)

(defn solve-part-2 [xs]
  (let [seen (atom {})
        target (+ (reduce max xs) 3)
        numbers (sort < (conj xs 0 target))
        walk (fn walk [numbers value]
               (if (contains? @seen value)
                 (get @seen value)
                 (let [available-numbers (drop-while #(<= % value) numbers)
                       opens (take-while #(<= % (+ value 3)) available-numbers)]
                   (cond
                     (empty? opens) (if (= value target) 1 0)
                     :else (let [result
                                 (reduce + (map (partial walk available-numbers) opens))]
                             (swap! seen assoc value result)
                             result)))))]
    (walk numbers (first numbers))))


(solve-part-2 sample-input)
(solve-part-2 sample-input2)
(solve-part-2 real-input)