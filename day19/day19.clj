(require '[clojure.string :as str]
         '[instaparse.core :as insta])

;; the rules valid messages should obey
;; list of received messages

"0: 1 2
1: \"a\"
2: 1 3 | 3 1
3: \"b\""

;; 0: 1 2
;; Rule 0 : Rule 1 then rule 2

;; 2: 1 3 | 3 1
;; At least one

;; Rule 2 -> ab or ba
;; Rule 0 -> aab or aba

(comment
  "could be use directly as a grammar in Instaparse"

  ((insta/parser
    "0: 1 2
1: \"a\"
2: 1 3 | 3 1
3: \"b\"") "aba")
  )

(def sample-input "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
  4: \"a\"
  5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb")
(def real-input (slurp "day19/input"))

(defn sort-rules [rules]
  (->> rules
       (re-seq #"(\d+): .*")
       (map (fn [[rule index]] [rule (read-string index)]))
       (sort-by second)
       (map first)
       (str/join "\n")))

(defn solve-part-1 [input]
  (let [[rules messages] (str/split input #"\R\R")
        parse (insta/parser (sort-rules rules))
        valid? (comp not insta/failure? parse)]
    (count (filter valid? (str/split-lines messages)))))

(solve-part-1 sample-input)
(solve-part-1 real-input)
