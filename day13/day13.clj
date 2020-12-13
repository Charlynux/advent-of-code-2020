;; Each bus has an ID number that also indicates how often the bus leaves for the airport.
;; At timestamp 0, every bus simultaneously departed from the sea port.
;; ex. ID 5 departs from the sea port at timestamps 0, 5, 10, 15

;; earliest timestamp you could depart on a bus
(require '[clojure.string :as str])

(def sample-input "939
7,13,x,x,59,x,31,19")
(def real-input (slurp "day13/input"))

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    [(read-string (first lines))
     (map read-string (str/split (second lines) #","))]))

(defn solve-part-1 [input]
  (let [[min-time bus] (parse-input input)
        times (remove #{'x} bus)]
    (loop [time min-time]
      (let [availables (filter #(zero? (mod time %)) times)]
        (if (empty? availables)
          (recur (inc time))
          (* (- time min-time) (apply min availables)))))))

(solve-part-1 sample-input)
(solve-part-1 real-input)

;; An x in the schedule means there are no constraints on what bus IDs must depart at that time.


;; 17,x,13,19
;; (mod n 17) = 0
;; (mod (+ n 2) 13) = 0

(defn pair [index value] (when (int? value) [value index]))

(defn step [[value index]] (fn [i] (- (* i value) index)))

(defn valid? [n pairs]
  (every? (fn [[value index]] (zero? (mod (+ index n) value))) pairs))

#_(time (let [input sample-input
            parsed-input (parse-input input)
            pairs (->> parsed-input
                       second
                       vec
                       (map-indexed pair)
                       (remove nil?))
            max-pair (last (sort-by first pairs))
            steps (map (step max-pair) (drop 1 (range)))]
        (first (filter #(valid? % pairs) steps))))
;; 20 seconds, with sample-input !!!!!!!
(require '[clojure.set :as set])

(defn find-meets
  "Debug function..."
  [[a x] [b y]]
  (let [meets (set/intersection
               (set (take 100 (iterate (partial + a) x)))
               (set (take 100 (iterate (partial + b) y))))]
    (prn meets)
    (prn "Min :" (apply min meets))
    (prn "Dist :" (map #(apply - %) (partition 2 1 (sort > meets))))))

(find-meets [7 0] [17 -1])
(find-meets [17 0] [13 -2])
(find-meets [19 3] [221 102])

;; modulo "all meets" distance = min meet of the two
(mod 1068781 91)

(defn gcd
  [a b]
  (loop [a a
         b b]
    (if (zero? a)
      b
      (recur (mod b a) a))))
(gcd 15 25)
(gcd 13 7)

(defn lcm [a b]
  (* (/ a (gcd a b)) b))
(lcm 15 25)
(lcm 13 7) ;; 91

(defn read-pairs [parsed]
  (->> parsed
       second
       vec
       (map-indexed pair)
       (remove nil?)))

(defn foo
  ([a-pair] a-pair)
  ([[a offset-a] [b offset-b]]
   (prn [a offset-a] [b offset-b])
   [(lcm a b)
    (time (+ (first (filter #(zero? (mod (- % (- offset-b offset-a)) b)) (iterate (partial + a) 0))) offset-a))]))

;; A pair store 2 informations :
;; - the step's length (when the bus get back to start point)
;; - the offset from first bus
;; The `foo` function "merges" two bus in a new one.
;; The new bus arrives at start point when the two merged bus arrived.
;; So step length = lcm(step lengths) and offset = first meeting of the two bus.
;;
;; This implies that you can call `foo` with a merged bus obtaining a bigger bus.
;; We do this until we get "full bus" merging all buses.

(defn solve-part-2 [input]
  (let [init-pairs (read-pairs (parse-input input))]
    (apply - (loop [pairs init-pairs]
               (if (= (count pairs) 1)
                 (first pairs)
                 (recur (map #(apply foo %) (partition-all 2 (sort-by second pairs)))))))))

(solve-part-2 "IGNORED\n17,x,13,19")
;; 3417
(solve-part-2 "IGNORED\n67,7,59,61")
;; 754018
(solve-part-2 "IGNORED\n67,x,7,59,61")
;; 779210
(solve-part-2 "IGNORED\n67,7,x,59,61")
;; 1261476
(solve-part-2 "IGNORED\n1789,37,47,1889")
;; 1202161486

(solve-part-2 sample-input)
;; 1068781

(time (solve-part-2 real-input))
;; 836024966345345
