(def sym->op
  {'+ +
   '- -
   '* *})

(defn eval [exp]
  (cond
    (int? exp) exp
    (seq? exp) (let [[a op-symbol b] exp]
                 ((sym->op op-symbol) (eval a) (eval b)))))

(defn precedence-rule [exprs]
  (loop [exprs exprs]
    (cond
      (not (seq? exprs)) exprs
      (= (count exprs) 1) (first exprs)
      :else (let [[a op b] (take 3 exprs)
                  next (drop 3 exprs)]
              (recur (cons (list (precedence-rule a) op (precedence-rule b)) next))))))

(defn parse [input]
  (precedence-rule (read-string (str "(" input ")"))))

(parse "1 + 3")
(parse "1 + 3 + 2")
(parse "1 + (3 + 2)")

(def math (comp eval parse))

(math "1 - 3 - 2")
;; => -4

(math "1 - (3 - 2)")
;; => 0

(math "2 * 3 + (4 * 5)")
(math "5 + (8 * 3 + 9 + 3 * 4 * 3)")
(math "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
(math "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

(->> (slurp "day18/input")
     clojure.string/split-lines
     (map math)
     (reduce + 0))
;; => 11076907812171
