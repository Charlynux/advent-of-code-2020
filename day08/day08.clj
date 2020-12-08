(require '[clojure.string :as str])

(def sample-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")
(def real-input (slurp "day08/input"))

(defn parse-instruction [line]
  (let [[_ instruction offset] (re-find #"(\w+) (.*)" line)]
    [instruction (Integer/parseInt offset)]))

(parse-instruction "acc -99")

(update {} 1 (fnil inc 0))

(defn program-step [{:keys [accumulator
                            current-index
                            program
                            seen] :as state}]
  (if (seen current-index)
    (reduced {:reason :cycle :value accumulator})
    (let [[inst offset] (get program current-index)]
      (case inst
          "acc" (->
                 state
                 (update :accumulator + offset)
                 (update :current-index inc)
                 (update :seen conj current-index))
          "jmp" (-> state
                    (update :current-index + offset)
                    (update :seen conj current-index))
          "nop" (-> state
                    (update :current-index inc)
                    (update :seen conj current-index))
          (reduced {:reason :finished :value accumulator})))))

(defn run-program [program]
  (let [state {:accumulator 0
               :current-index 0
               :program program
               :seen #{}}]
    (reduce
     (fn [acc _] (program-step acc))
     state
     (range))))

(defn solve-part-1 [input]
  (let [program (mapv parse-instruction (str/split-lines input))]
    (:value (run-program program))))

(solve-part-1 sample-input)
(solve-part-1 real-input)
;; 2034

(defn fix-program [program n]
  (update-in program [n 0] #(if (= "jmp" %) "nop" "jmp")))

(defn solve-part-2 [input]
  (let [program (mapv parse-instruction (str/split-lines input))]
    (loop [n 0]
      (if (= (get-in program [n 0]) "acc")
        (recur (inc n))
        (let [fixed-program (fix-program program n)
              result (run-program fixed-program)]
          (if (= :cycle (:reason result))
            (recur (inc n))
            result))))))

(solve-part-2 sample-input)
(solve-part-2 real-input)
