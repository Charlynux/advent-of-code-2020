(defn step [{:keys [previous turn values] :as state}]
  (let [seen (get values previous)
        value (if (< (count seen) 2)
                0
                (apply - (take 2 seen)))]
    (-> state
        (assoc :previous value)
        (update :turn inc)
        (update-in [:values value] #(cons turn %)))))

(defn solve-part-1 [initial-state]
  (first (map :previous (drop-while #(<= (:turn %) 2020) (iterate step initial-state)))))

(solve-part-1 {:previous 6 :turn 4 :values {0 [1], 3 [2], 6 [3]}})
(solve-part-1 {:previous 5 :turn 7 :values {0 [1],3 [2],1 [3],6 [4],7 [5],5 [6]}})
