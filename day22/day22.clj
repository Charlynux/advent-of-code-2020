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

(defn play-recursive-game [xs ys]
  (loop [xs xs
         ys ys
         rounds #{}]
    (cond
      (empty? xs) ['y ys]
      (empty? ys) ['x xs]
      (rounds [xs ys]) ['x xs]
      :else
      (let [x (first xs)
            y (first ys)
            xrest (vec (rest xs))
            yrest (vec (rest ys))
            winner (cond
                     (and (<= x (count xrest))
                          (<= y (count yrest)))
                     (first (play-recursive-game
                             (take x xrest)
                             (take y yrest)))

                     (< x y) 'y
                     (> x y) 'x)]
        (case winner
          y (recur
                   xrest
                   (conj yrest y x)
                   (conj rounds [xs ys]))
          x (recur
             (conj xrest x y)
                   yrest
                   (conj rounds [xs ys])))))))

(play-recursive-game [43 19] [2 29 14])
(play-recursive-game [9, 2, 6, 3, 1] [5, 8, 4, 7, 10])

(let [decks (map parse-deck (str/split (slurp "day22/input") #"\R\R"))]
  (calculate-score (second (apply play-recursive-game decks))))
;; 31793
