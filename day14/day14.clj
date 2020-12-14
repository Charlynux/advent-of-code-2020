(require '[clojure.string :as str])

(def real-input (slurp "day14/input"))

(defn parse-mask [mask]
  (map-indexed (fn [i v] [i v])
               (reverse (last (re-matches #"([X01]{36})" mask)))))

(defn use-mask [mask n]
  (reduce (fn [n [i v]]
            (case v
              \0 (bit-clear n i)
              \1 (bit-set n i)
              n)) n mask))

(def sample-mask (parse-mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"))
(use-mask sample-mask 11)
(use-mask sample-mask 101)
(use-mask sample-mask 0)

(defn parse-mem [mem-line]
  (mapv read-string
        (rest (re-matches #"mem\[(\d+)\] = (\d+)" mem-line))))

(parse-mem "mem[20690] = 435")

(re-matches #"mask = ([01X]+).*" real-input)

(defn parse-group [group]
  (let [[mask & mems] (str/split-lines group)]
    [(parse-mask mask) (map parse-mem mems)]))

(def parsed-input
  (let [groups (rest (str/split real-input #"mask = "))]
    (map parse-group groups)))

(defn execute-group [[mask mems]]
  (into {}
        (map (fn [[index value]] [index (use-mask mask value)]))
        mems))

(reduce + (vals (into {} (map execute-group) parsed-input)))
;; 4297467072083

(defn execute-group [[mask mems]]
  (into {}
        (map (fn [[index value]] [index (use-mask mask value)]))
        mems))

(def sample [[(parse-mask "000000000000000000000000000000X1001X")
              [[42 100]]]
             [(parse-mask "00000000000000000000000000000000X0XX")
              [[26 1]]]])

(defn change-bit [n [i v]]
  (case v
    \0 [n]
    \1 [(bit-set n i)]
    \X [(bit-set n i) (bit-clear n i)]))

(defn use-mask-2 [mask n]
  (reduce (fn [ns m] (mapcat #(change-bit % m) ns)) [n] mask))

(use-mask-2
 (parse-mask "000000000000000000000000000000X1001X")
 42)

(use-mask-2
 (parse-mask "00000000000000000000000000000000X0XX")
 26)

(defn execute-group-2 [[mask mems]]
  (into {}
        (mapcat (fn [[index value]]
                  (for [address (use-mask-2 mask index)]
                     [address value])))
        mems))

(reduce + (vals (into {} (map execute-group-2 sample))))

(reduce + (vals (into {} (map execute-group-2 parsed-input))))
;; => 5030603328768
