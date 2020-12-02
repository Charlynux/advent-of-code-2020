(def datas (->> (slurp "day01/input")
                (clojure.string/split-lines)
                (map #(Long/parseLong %))))

(defn solve-part1 [values]
  (->>
   (for [a values
         b values
         :when (not= a b)
         :when (= 2020 (+ a b))]
     [a b])
   first
   (apply *)))

(solve-part1 datas)

(defn solve-part2 [values]
  (->>
   (for [a values
         b values
         c values
         :when (not= a b c)
         :when (= 2020 (+ a b c))]
     [a b c])
   first
   (apply *)))

(solve-part2 datas)
