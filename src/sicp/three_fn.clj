(ns sicp.three-fn)
(defn three-fn [n]
  (if
   (< n 3)
    n
    (+ (three-fn (- n 1)) (* 2 (three-fn (- n 2))) (* 3 (three-fn (- n 3))))))

(defn three-fn-iter [a b c count]
  (if
   (= count 0)
    c
    (three-fn-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(defn three-linear-iteration [n]
  (if
   (< n 3)
    n
    (three-fn-iter 2 1 0 n)))
