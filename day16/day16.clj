(require '[clojure.string :as str]
         '[clojure.set :as set])

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

(def sample-input-2 "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9")

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
     :ticket (first (map parse-ticket (drop 1 (str/split-lines ticket))))
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

(defn valid-class? [n [_ ranges]]
  (some
   (fn [[low high]] (<= low n high))
   ranges))

(defn find-classes [classes ticket]
  (for [n ticket]
    (set (map first (filter #(valid-class? n %) classes)))))

(defn clean-classes [classes]
  (loop [classes classes]
    (let [sures (into #{} (comp (filter #(= (count %) 1)) cat) classes)
          new-classes (map #(if (= 1 (count %))
                              %
                              (set/difference % sures)) classes)]
      (if (= classes new-classes)
        new-classes
        (recur new-classes)))))

(clean-classes '(#{:row :seat} #{:seat :class} #{:class}))

(defn get-ticket-map [{:keys [classes ticket nearby]}]
  (let [pos-classes (->>
                     nearby
                     (map #(find-classes classes %))
                     (remove #(some empty? %))
                     (cons (find-classes classes ticket))
                     (apply map set/intersection))
        pos->class (map first (clean-classes pos-classes))]
    (zipmap pos->class ticket)))

(get-ticket-map (parse-input sample-input-2))

(let [ticket (get-ticket-map (parse-input real-input))
      ks (filter #(.startsWith (name %) "departure") (keys ticket))]
  (apply * (map ticket ks)))
;; => 279139880759
