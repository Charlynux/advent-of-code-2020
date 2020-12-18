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

(defn advanced-precedence-group [exprs]
  (loop [previous nil
         [a op] (take 2 exprs)
         next (drop 2 exprs)]
    (cond
      (= op '+) [previous [a op (first next)] (rest next)]
      (= 1 (count next)) [nil (take 3 exprs) (drop 3 exprs)]
      :else (recur (concat previous [a op]) (take 2 next) (drop 2 next)))))

(advanced-precedence-group (list 1 '* 3 '+ 2))

(defn advanced-precedence-rule [exprs]
  (loop [exprs exprs]
    (cond
      (not (seq? exprs)) exprs
      (= (count exprs) 1) (first exprs)
      :else (let [[before [a op b] after] (advanced-precedence-group exprs)]
              (recur (concat before (cons (list (advanced-precedence-rule a) op (advanced-precedence-rule b)) after)))))))

(defn advanced-parse [input]
  (advanced-precedence-rule (read-string (str "(" input ")"))))

(eval (advanced-parse "1 + 2 * 3 + 4 * 5 + 6"))

(def advanced-math (comp eval advanced-parse))

(advanced-math "1 + (2 * 3) + (4 * (5 + 6))")
(advanced-math "2 * 3 + (4 * 5)")
(advanced-math "5 + (8 * 3 + 9 + 3 * 4 * 3)")

(->> (slurp "day18/input")
     clojure.string/split-lines
     (map advanced-math)
     (reduce + 0))
;; => 283729053022731
