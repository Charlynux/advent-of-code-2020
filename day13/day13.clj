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
