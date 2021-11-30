(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defvar xs (get-file "./git-repositories/advent-of-code-2020/day01/input"))
(defvar ns (mapcar (lambda (n) (parse-integer n)) xs))

;; Solution 1
(let ((pairs (loop for i in ns
                   append (loop for j in ns
                                when (and (< i j) (not (eq i j)))
                                  when (= 2020 (+ i j))
                                    collect (cons i j)))))
  (* (caar pairs) (cdar pairs)))

;; Solution 2
(let ((pairs (loop for i in ns
                   append (loop for j in ns
                                append (loop for k in ns
                                             when (and (not (eq i j)) (not (eq j k)) (not (eq i k)) )
                                               when (= 2020 (+ i j k))
                                                 collect (cons i (cons j k)))))))
  (* (caar pairs) (cadar pairs) (cddar pairs)))
