(require '[clojure.string :as str])

(defn parse-input [input]
  (let [values (str/split input #",")]
    {:previous (last values)
     :turn (inc (count values))
     :values (into {} (map-indexed (fn [i v] [(read-string v) (inc i)]) values))}))

(defn step [{:keys [previous turn values] :as state}]
  (let [value (- (dec turn) (get values previous (dec turn)))]
    (-> state
        (assoc :previous value)
        (update :turn inc)
        (assoc-in [:values previous] (dec turn)))))

(defn find-nth [n initial-state]
  (first (map :previous (drop-while #(<= (:turn %) n) (iterate step initial-state)))))

(find-nth 2020 (parse-input "0,3,6"))
(find-nth 2020 (parse-input "0,3,1,6,7,5"))

(time (find-nth 30000000 (parse-input "0,3,6")))
;; "Elapsed time: 111178.717471 msecs"
;; 175594

(find-nth 30000000 (parse-input "1,3,2"))
(find-nth 30000000 (parse-input "2,1,3"))
(find-nth 30000000 (parse-input "1,2,3"))
(find-nth 30000000 (parse-input "2,3,1"))
(find-nth 30000000 (parse-input "3,2,1"))
(find-nth 30000000 (parse-input "3,1,2"))

(find-nth 30000000 (parse-input "0,3,1,6,7,5"))
;; => 6007666
;; Not submitted since I don't think to just run it since I saw @sophiebits solution.
