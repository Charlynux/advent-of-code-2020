(require '[clojure.string :as str])

(def sample-input "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(def real-input (slurp "day16/input"))

(defn parse-class [input]
  (let [[_ cls & nums] (re-matches #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" input)]
    [(keyword (str/replace cls #" " "_"))
     (mapv vec (partition 2 (map read-string nums)))]))

(parse-class "class: 1-3 or 5-7")
(parse-class "another class: 1-3 or 5-7")

(defn parse-ticket [input]
  (map read-string (str/split input #",")))

(parse-ticket "7,3,47")

(defn parse-input [input]
  (let [[classes ticket nearby] (str/split input #"\R\R")]
    {:classes (map parse-class (str/split-lines classes))
     :nearby (map parse-ticket (drop 1 (str/split-lines nearby)))}))

(parse-input sample-input)

(defn valid? [classes n]
  (let [all-ranges (mapcat second classes)]
    (some (fn [[low high]] (<= low n high)) all-ranges)))

(defn invalids [classes ticket]
  (remove (partial valid? classes) ticket))

(defn error-rate [classes nearby]
  (reduce + 0 (mapcat (partial invalids classes) nearby)))

(defn solve-part-1 [input]
  (let [{:keys [classes nearby]} (parse-input input)]
    (error-rate classes nearby)))

(solve-part-1 sample-input)
(solve-part-1 real-input)
