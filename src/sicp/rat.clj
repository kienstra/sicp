(ns sicp.rat)

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

(defn neg [n]
  (- 0 (abs n)))

(defn make-rat [n d]
  (let [g (gcd n d)]
    [(/ n g) (/ d g)]))

(defn make-rat-pos-neg [n d]
  (let [g (gcd n d)]
    [((if (pos? (/ n d)) abs neg) (/ n g)) (abs (/ d g))]))

(defn numer [x] (first x))
(defn denom [x] (last x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))

(defn print-rat [rat]
  (println (numer rat) "/" (denom rat)))

(defn make-point [x y]
  [x y])
(defn x-point [p]
  (first p))
(defn y-point [p]
  (last p))

(defn average [& args]
  (/ (reduce + args) (count args)))

(defn midpoint-segment [p q]
  [(average (x-point p) (x-point q)) (average (y-point p) (y-point q))])

(defn print-point [p]
  (println "(" (x-point p) "," (y-point p) ")"))
