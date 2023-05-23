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

(defn product-lin [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product-lin term (next a) next b))))

(defn product-iter [term a next b]
  ((fn [a result next b]
     (if (> a b)
       result
       (recur (next a) (* result (term a)) next b)))
   a
   1
   next
   b))

(defn factorial [n]
  (product-lin identity 1 inc n))
