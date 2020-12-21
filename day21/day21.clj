(require '[clojure.string :as str]
         '[clojure.set :as set])

(def sample-input "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(def real-input (slurp "day21/input"))

(defn parse-line [line]
  (let [[_ ingredients allergens]
        (re-matches #"(.*) \(contains (.+)\)" line)]
    [(set (str/split ingredients #" "))
     (str/split allergens #", ")]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn index-by-allergens [recipes]
  (->> recipes
       (map (fn [[ingredients allergens]]
              (reduce
               (fn [acc allergen] (assoc acc allergen (set ingredients)))
               {}
               allergens)))
       (apply merge-with set/intersection)))

(defn solve-part-1 [input]
  (let [recipes (parse-input input)
        all-ingredients (->> recipes
                             (mapcat first))
        contains-allergens (apply set/union (vals (index-by-allergens recipes)))
        no-allergens (set/difference
                      (set all-ingredients)
                      contains-allergens)]
    (reduce + (map (frequencies all-ingredients) no-allergens))))

(solve-part-1 sample-input)
(solve-part-1 real-input)

;; Code taken from day16
(defn remove-singles [index]
  (loop [classes index]
    (let [sures (into #{} (comp (filter #(= (count %) 1)) cat) (vals classes))
          new-classes (into {} (map (fn [[k vs]] (if (= 1 (count vs))
                                                  [k vs]
                                                  [k (set/difference vs sures)])) classes))]
      (if (= classes new-classes)
        new-classes
        (recur new-classes)))))

(defn solve-part-2 [input]
  (let [recipes (parse-input input)
        index (index-by-allergens recipes)]
    (str/join "," (map (comp first second) (sort-by first (remove-singles index))))))

(solve-part-2 sample-input)
(solve-part-2 real-input)
