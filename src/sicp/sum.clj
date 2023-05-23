(ns sicp.sum)

(defn sum-lin [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum-lin term (next a) next b))))

(defn sum-iter [term a next b]
  ((fn [a result next b]
     (if (> a b)
       result
       (recur (next a) (+ result (term a)) next b)))
   a
   0
   next
   b))
