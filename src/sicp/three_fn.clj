(ns sicp.three-fn)
(defn three-fn [n]
  (if 
    (< n 3) 
    n
    (+ (three-fn (- n 1)) (* 2 (three-fn (- n 2))) (* 3 (three-fn (- n 3))))))
