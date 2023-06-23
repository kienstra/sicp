(ns sicp.digits)

(defn digits-iter [x y accum]
  (let [highest-order-x (Math/floor (Math/log10 x))
        highest-order-y (Math/floor (Math/log10 y))
        amount-in-order (+ 1 (- y (Math/pow 10 highest-order-y)))]
    (if
     (or
      (= highest-order-x highest-order-y)
      (and (= 0 highest-order-y) (= '##-Inf highest-order-x)))
      (int (+ accum (- (- y x) (Math/pow 10 highest-order-y))))
      (digits-iter
       x
       (- y amount-in-order)
       (+ accum (* (+ 1 highest-order-y) amount-in-order))))))
(defn digits [x y]
  (digits-iter x (- y 1) 0))
