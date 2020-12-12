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
  (let [index (.indexOf DIRECTIONS direction)
        offset (/ angle 90)]
    (nth DIRECTIONS (mod (+ index offset) (count DIRECTIONS)))))

(defn turn-left [angle direction]
  (turn-right (- angle) direction))

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
