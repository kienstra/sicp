(ns sicp.symbolic-data)

(defn variable? [x] (symbol? x))
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))
(defn =number? [exp num]
  (and (number? exp) (= exp num)))
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (or (=number? a2 0) (= '() a2) (every? nil? a2)) a1
        (and (number? a1) (number? a2)) (list '+ a1 a2)
        (and (seq? a2) (rest a2)) (list '+ a1 (make-sum (first a2) (rest a2)))
        :else (list '+ a1 a2)))
(defn make-subtraction [a1 a2]
  (cond (or (=number? a1 0) (nil? a1)) (- a2)
        (or (=number? a2 0) (nil? a2)) a1
        (and (seq? a2) (every? nil? a2)) a1
        (and (number? a1) (number? a2)) (- a1 a2)
        :else (list '- a1 a2)))
(defn make-product [m1 m2]
  (cond (or
         (=number? m1 0) (=number? m2 0))
        0
        (= '() m2) m1
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (seq? m2) (every? nil? m2)) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        (and (seq? m2) (rest m2)) (list '* m1 (make-product (first m2) (rest m2)))
        :else (list '* m1 m2)))
(defn pair? [x] (and (list? x) (= (count x) 3)))
(defn at-least-pair? [x]
  (and (list? x) (>= (count x) 3)))
(defn sum? [x]
  (and (at-least-pair? x) (= (first x) '+)))
(defn addend [s] (nth s 1))
(defn augend [s]
  (let [[_ _ b & args] s]
    (make-sum b args)))
(defn product? [x]
  (and (at-least-pair? x) (= (first x) '*)))
(defn multiplier [p] (nth p 1))
(defn multiplicand [p]
  (let [[_ _ b & args] p]
    (make-product b args)))

(defn exponentiation? [x]
  (and (pair? x) (= (first x) '**)))
(defn base [s] (nth s 1))
(defn exponent [s] (nth s 2))
(defn make-exponentiation [m1 m2]
  (cond (=number? m2 0) 1
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (Math/pow m1 m2)
        :else (list '** m1 m2)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp)
        (if (same-variable? exp var) 1 0)
        (sum? exp)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var))
        (product? exp)
        (make-sum
         (make-product (multiplier exp)
                       (deriv (multiplicand exp) var))
         (make-product (deriv (multiplier exp) var)
                       (multiplicand exp)))
        (exponentiation? exp)
        (make-product (base exp) (exponent exp))
        :else
        {:error "unknown expression type -- DERIV"}))
