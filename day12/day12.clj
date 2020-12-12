(def sample-input "F10
  N3
  F7
  R90
  F11")
(def real-input (slurp "day12/input"))


(defn parse-input [input]
  (for [[_ action value] (re-seq #"(\w)(\d+)" input)]
                           [(keyword action) (Long/parseLong value)]))

(def action->direction { :N [0 1] :S [0 -1] :W [-1 0] :E [1 0] })
(def DIRECTIONS (mapv action->direction [:E :S :W :N]))

(def initial-state {:direction (action->direction :E) :position [0 0]})

(defn move
  ([direction position] (mapv + direction position))
  ([n direction position] (move (mapv #(* n %) direction) position)))

(defn turn-right [angle direction]
  (let [offset (/ angle 90)
        turn (fn [[x y]] [y (- x)])]
    (nth (iterate turn direction) offset)))

(defn turn-left [angle direction]
  (let [offset (/ angle 90)
        turn (fn [[x y]] [(- y) x])]
    (nth (iterate turn direction) offset)))

(turn-right 90 [1 0])
;; [0 -1]
(turn-right 90 [10 4])
;; [4 -10]

(turn-left 90 [1 0])

(defn apply-actions [{:keys [position direction] :as state} [action value]]
  (cond
    (#{:N :S :E :W} action) (update state :position #(move value (action->direction action) %))
    (= action :F) (update state :position #(move value direction %))
    (= action :R) (update state :direction #(turn-right value %))
    (= action :L) (update state :direction #(turn-left value %))
    :else state))

(defn manhattan-distance [a b]
  "d = |xa - xb| + |ya - yb| + |za - zb|"
  (let [abs-sub #(Math/abs (- %1 %2))]
    (reduce + (map abs-sub a b))))

(manhattan-distance [0 0] [17 -8])

(reduce apply-actions
        initial-state
        (parse-input sample-input))

(reduce apply-actions
        initial-state
        (parse-input real-input))

(defn solve-part-1 [input]
  (->>
   input
   parse-input
   (reduce apply-actions initial-state)
   :position
   (manhattan-distance [0 0])))

(solve-part-1 sample-input)
(solve-part-1 real-input)

(defn apply-actions-2 [{:keys [position waypoint] :as state} [action value]]
  (cond
    ;; move waypoint
    (#{:N :S :E :W} action) (update state :waypoint #(move value (action->direction action) %))
    (= action :R) (update state :waypoint #(turn-right value %))
    (= action :L) (update state :waypoint #(turn-left value %))
    ;; move ship
    (= action :F) (update state :position #(move value waypoint %))
    :else state))

(def initial-state-2 {:waypoint [10 1] :position [0 0]})

(reduce apply-actions-2 initial-state-2 [[:F 10] [:N 3] [:F 7] [:R 90]])

(defn solve-part-2 [input]
  (->>
   input
   parse-input
   (reduce apply-actions-2 initial-state-2)
   :position
   (manhattan-distance [0 0])))

(solve-part-2 sample-input)
(solve-part-2 real-input)
