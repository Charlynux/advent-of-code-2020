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

(defn sort-rules [rules] (str "valid : 0\n" rules))

(defn solve-part-1 [input]
  (let [[rules messages] (str/split input #"\R\R")
        parse (insta/parser (sort-rules rules))
        valid? (comp not insta/failure? parse)]
    (count (filter valid? (str/split-lines messages)))))

(solve-part-1 sample-input)
(solve-part-1 real-input)

(defn fix-rules [s]
  (-> s
      (str/replace #"8: 42" "8: 42 | 42 8")
      (str/replace #"11: 42 31" "11: 42 31 | 42 11 31")))

(defn solve-part-2 [input]
  (let [[rules messages] (str/split input #"\R\R")
        parse (insta/parser (sort-rules (fix-rules rules)))
        valid? (comp not insta/failure? parse)]
    (count (filter valid? (str/split-lines messages)))))

(def sample-input2
  "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")

(solve-part-1 sample-input2)
(solve-part-2 sample-input2)
(solve-part-2 real-input)
