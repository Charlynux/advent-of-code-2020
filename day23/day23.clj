(defn parse-input [input] (map read-string (clojure.string/split input #"")))

(def sample-input (parse-input "389125467"))
(def real-input (parse-input "398254716"))

(defn find-destination-value [cups current pick-up]
  (let [highest (apply max cups)
        picked? (set pick-up)
        rolling-dec (fn [n] (let [m (dec n)]
                              (if (<= m 0) highest m)))]
    (loop [n (rolling-dec current)]
      (if (picked? n)
        (recur (rolling-dec n))
        n))))

(defn step [cups]
  (let [current (first cups)
        pick-up (->> cups (drop 1) (take 3))
        remain (drop 4 cups)
        destination (find-destination-value cups current pick-up)]
    (concat
     (take-while #(not= % destination) remain)
     (list destination)
     pick-up
     (drop 1 (drop-while #(not= % destination) remain))
     (list current))))

(step sample-input)
(step (step sample-input))
(take 10 (iterate step sample-input))

(defn read-label [cups]
  (->> cups
       cycle
       (drop-while #(not= 1 %))
       rest
       (take 8)
       (clojure.string/join "")))

(read-label '(5 8 3  7  4  1  9  2  6))

(read-label (nth (iterate step sample-input) 100))

(read-label (nth (iterate step real-input) 100))
;; => "45798623"
