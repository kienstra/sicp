(ns sicp.interval)

(defn make-interval [a b]
  [a b])

(defn lower-bound [i]
  (first i))

(defn upper-bound [i]
  (last i))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn sub-interval [x y]
  (make-interval (abs (- (lower-bound x) (lower-bound y)))
                 (abs (- (upper-bound x) (upper-bound y)))))

(defn div-interval [x y]
  (if (= (upper-bound y) (lower-bound y))
    {:error "Cannot divide by an interval that spans 0"}
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(defn list-ref [items n]
  (if (= n 0)
    (first items)
    (list-ref (rest items) (- n 1))))

(defn last-item [x]
  (if (= 1 (count x))
    (first x)
    (recur (rest x))))

(defn reverse-iter [forward reversed]
  (if (= 0 (count forward))
    reversed
    (recur (rest forward) (cons (first forward) reversed))))

(defn reverse-list [x]
  (reverse-iter x '()))

(defn reverse-iter-list [forward reversed]
  (if (= 0 (count forward))
    reversed
    (recur
     (rest forward)
     (cons
      (if (list? (first forward))
        (reverse-iter-list (first forward) '())
        (first forward))
      reversed))))

(defn deep-reverse-list [x]
  (reverse-iter-list x '()))

(defn same-parity [x & args]
  (cons x (filter (if (odd? x) odd? even?) args)))

(defn scale-list [x factor]
  (map factor x))

(defn square-list [x]
  (map #(* % %) x))

(defn square [x]
  (* x x))

(defn square-list-cons [items]
  (if (empty? items)
    nil
    (conj (square-list-cons (rest items)) (square (first items)))))

(defn for-each [fn li]
  (if (empty? li)
    true
    (do
      (fn (first li))
      (recur fn (rest li)))))

(defn fringe-iter [x flattened]
  (if (seq x)
    (recur (rest x) (into flattened (first x)))
    flattened))

(defn fringe [x]
  (fringe-iter x []))
