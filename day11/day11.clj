(require '[clojure.string :as str])

(def sample-input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def real-input (slurp "day11/input"))

(defn parse-input [input]
  (into {}
        (comp
         (map-indexed
          (fn [y row]
            (map-indexed (fn [x value]
                           [[x y] value]) row)))
         cat
         (remove (fn [[pos value]] (= \. value))))
        (str/split-lines input)))

(def DIRECTIONS #{[0 1] [1 0] [0 -1] [-1 0] [1 1] [-1 -1] [-1 1] [1 -1]})

(defn neighbors [[a b]]
  (mapv (fn [[x y]] [(+ a x) (+ b y)]) DIRECTIONS))

(defn debug-layout [layout]
  (prn "--------------------------------------------------------------")
  (let [width (apply max (map first (keys layout)))
        height (apply max (map second (keys layout)))]
    (doseq [x (range (inc width))]
      (doseq [y (range (inc height))]
        (pr (layout [y x])))
      (prn))))

(defn count-seats [position layout]
  (frequencies (keep layout (neighbors position))))

(defn generation [layout]
  (into {}
        (map
           (fn [[pos v]]
             (let [new-v (cond
                           (and (= \L v) (zero? (get (count-seats pos layout) \# 0))) \#
                           (and (= \# v) (<= 4 (get (count-seats pos layout) \# 0))) \L
                           :else v)]
               [pos new-v]))
           layout)))

(defn stale? [previous x]
  (if (= x previous)
    (reduced x)
    x))

(defn count-occupied [layout]
  (count (filter #{\#} (vals layout))))

(defn solve-part-1 [input]
  (count-occupied (reduce stale? (iterate generation (parse-input input)))))

(solve-part-1 sample-input)
(solve-part-1 real-input)
