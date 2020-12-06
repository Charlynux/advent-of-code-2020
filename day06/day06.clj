(require '[clojure.set :as set]
         '[clojure.string :as str])


(def sample-input "abc

a
b
c

ab
ac

a
a
a
a

b")

(defn split-groups [input]
  (str/split input #"\R\R"))

(defn read-group-responses [group]
  (into
   #{}
   (re-seq #"[a-z]" group)))

(defn solve-part-1 [input]
  (->> input
       split-groups
       (map read-group-responses)
       (map count)
       (reduce +)))

(solve-part-1 sample-input)

(solve-part-1 (slurp "day06/input"))

(defn read-group-responses-2 [group]
  (reduce set/intersection (map set (str/split-lines group))))

(defn solve-part-2 [input]
  (->> input
       split-groups
       (map read-group-responses-2)
       (map count)
       (reduce +)))

(solve-part-2 sample-input)

(solve-part-2 (slurp "day06/input"))
