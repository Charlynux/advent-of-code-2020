(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-input [input]
  (into #{}
        (comp
         (map-indexed
          (fn [y row]
            (map-indexed (fn [x value]
                           (when (not= \. value)
                             [x y 0])) row)))
         cat
         (filter some?))
        (str/split-lines input)))

(def sample-input
  ".#.
..#
###")

(def real-input (slurp "day17/input"))

(defn get-bounds [n layout]
  (apply (juxt min max) (map #(nth % n) layout)))

(defn debug-layout [layout]
  (prn "--------------------------------------------------------------")
  (let [[min-y max-y] (get-bounds 0 layout)
        [min-x max-x] (get-bounds 1 layout)
        [min-z max-z] (get-bounds 2 layout)]
    (doseq [z (range min-z (inc max-z))]
      (println "z=" z)
      (doseq [y (range min-y (inc max-y))]
        (doseq [x (range min-x (inc max-x))]
          (print (if (get layout [y x z]) \# \.)))
        (prn)))))

#_(debug-layout (parse-input sample-input))

(defn find-neighbors [[x y z]]
  (for [x' (range -1 2)
        y' (range -1 2)
        z' (range -1 2)
        :when (not= 0 x' y' z')]
    [(+ x x') (+ y y') (+ z z')]))

(count (find-neighbors [0 0 0])) ;; 26

(defn generation [actives]
  (let [neighbors-count (frequencies (mapcat find-neighbors actives))]
    (into
     #{}
     (comp
      (map (fn [[coords nb]]
             (when (or
                    (and (actives coords)
                         (#{2 3} nb))
                    (and (not (actives coords))
                         (= 3 nb))) coords)) )
      (filter some?))
     neighbors-count)))

(defn solve-part-1 [input]
  (count (nth (iterate generation (parse-input input)) 6)))

(solve-part-1 sample-input)
(solve-part-1 real-input)
