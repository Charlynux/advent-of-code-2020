(require '[clojure.string :as str])

(defn play-game [xs ys]
  (loop [xs xs
         ys ys]
    (cond
      (empty? xs) ys
      (empty? ys) xs
      :else
      (let [x (first xs)
            y (first ys)]
        (cond
          (< x y) (recur (vec (rest xs)) (conj (vec (rest ys)) y x))
          (> x y) (recur (conj (vec (rest xs)) x y) (vec (rest ys))))))))

(defn calculate-score [xs]
  (loop [score 0
         n (count xs)
         xs xs]
    (if (empty? xs)
      score
      (recur (+ score (* n (first xs)))
             (dec n)
             (rest xs)))))

(calculate-score [3, 2, 10, 6, 8, 5, 9, 4, 7, 1])

(defn parse-deck [deck]
  (->> deck
       str/split-lines
       rest
       (map read-string)
       vec))

(let [decks (map parse-deck (str/split (slurp "day22/input") #"\R\R"))]
  (calculate-score (apply play-game decks)))
