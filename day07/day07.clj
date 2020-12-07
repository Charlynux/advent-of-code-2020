(require '[clojure.string :as str]
         '[clojure.set :as set])

(def sample-input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn parse-content [content]
  (->> content
       (re-seq #"(\d+) (\w+ \w+) bag")
       (map rest)
       (map (fn [[n colour]] [(Long/parseLong n) colour]))))

(parse-content "no other bags.")
(parse-content "5 faded blue bags, 6 dotted black bags.")

(defn parse-bag [bag]
  (let [[_ colour] (re-find #"(\w+ \w+) bags" bag)]
    colour))

(parse-bag "dark orange bags")

(defn parse-line [line]
  (let [[bag content] (str/split line #" contain ")]
    [(parse-bag bag)
     (parse-content content)]))

(parse-line "bright white bags contain 1 shiny gold bag.")

(defn index-by-content [bags]
  (apply merge-with
         set/union
         (mapcat (fn [[bag content]]
                   (map
                    (fn [[_ colour]] (hash-map colour #{bag}))
                    content)) bags)))

(defn solve-part-1 [input]
  (let [bags (map parse-line (str/split-lines input))
        content->bags (index-by-content bags)]
    (->> "shiny gold"
         (tree-seq
          (constantly true)
          #(get content->bags % #{}))
         rest
         set
         count)))

(solve-part-1 sample-input)
(solve-part-1 (slurp "day07/input"))

(defn solve-part-2 [input]
  (let [bags (map parse-line (str/split-lines input))
        bag->content (into {} bags)
        rec (fn rec [[value colour]]
              (let [children (get bag->content colour [])]
                (if (empty? children)
                  value
                  (+ value (* value (reduce + (map rec children)))))))]
    (dec (rec [1 "shiny gold"]))))

(solve-part-2 sample-input)
(solve-part-2 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")
(solve-part-2 (slurp "day07/input"))
