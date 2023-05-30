(ns sicp.symbolic-data)

(defn variable? [x] (symbol? x))
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))
(defn =number? [exp num]
  (and (number? exp) (= exp num)))
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))
(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))
(defn pair? [x] (and (list? x) (= 3 (count x))))
(defn sum? [x]
  (and (pair? x) (= (first x) '+)))
(defn addend [s] (nth s 1))
(defn augend [s] (nth s 2))
(defn product? [x]
  (and (pair? x) (= (first x) '*)))
(defn multiplier [p] (nth p 1))
(defn multiplicand [p] (nth p 2))

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
        :else
        {:error (str "unknown expression type -- DERIV" exp)}))
