;; subject number = value + op * loop size

(defn transform [subject-number value]
  (mod (* value subject-number)
       20201227))

(defn transform-subject-number
  [subject-number loop-size]
  (nth (iterate (partial transform subject-number) 1) loop-size))


;; Input
;; (transform-subject-number 7 card-loop-size)
;; (transform-subject-number 7 door-loop-size)

(def real-input [14082811 5249543])

(defn find-loop-size [public-key]
  (->>
   (iterate (partial transform 7) 1)
   (take-while #(not= % public-key))
   count))
(find-loop-size 5764801)
(find-loop-size 17807724)

(let [[card-public-key door-public-key] real-input
      card-loop-size (find-loop-size card-public-key)]
  (transform-subject-number door-public-key card-loop-size))
;; 3217885
