(ns sicp.digits)

(defn digits-iter [x y accum]
  (let [order-x (Math/floor (Math/log10 x))
        order-y (Math/floor (Math/log10 y))
        amount-order-y (+ 1 (- y (Math/pow 10 order-y)))
        digits-order-y (+ 1 order-y)]
    (cond
      (>= x y)
      (bigint accum)
      (= order-x order-y)
      (bigint (+ accum (* digits-order-y (- y x))))
      (and (zero? order-y) (zero? x))
      (bigint (+ accum y))
      :else (recur
             x
             (- y amount-order-y)
             (+ accum (* digits-order-y amount-order-y))))))

(defn digits [x y]
  (digits-iter x (- y 1) 0))
