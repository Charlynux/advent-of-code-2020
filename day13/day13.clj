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

(time (let [input sample-input
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
