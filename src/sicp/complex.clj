(ns sicp.complex)

(defn variable? [x] (symbol? x))
(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))
(defn operand? [exp]
  (or (variable? exp) (number? exp)))
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(def table (ref {}))
(defn get-operation [op type]
  (get (get (deref table) op {}) type))
(defn put-operation! [op type item]
  (dosync
   (alter table (fn [previous-table]
                  (into previous-table
                        {op (into (get previous-table op {})
                                  {type item})})))))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (or (=number? a2 0) (= '() a2) (nil? a2)) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        (and (seq? a2) (rest a2)) (list '+ a1 (make-sum (first a2) (rest a2)))
        :else (list '+ a1 a2)))
(defn addend [s] (first s))
(defn augend [s]
  (let [[_ b & args] s]
    (make-sum b args)))
(defn make-product [m1 m2]
  (cond (or
         (=number? m1 0) (=number? m2 0))
        0
        (= '() m2) m1
        (nil? m2) m1
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        (and (not (operand? (first m2))) (seq? m2) (rest m2)) (list '* m1 (make-product (first m2) (rest m2)))
        :else (list '* m1 m2)))
(defn multiplier [p] (first p))
(defn multiplicand [p]
  (let [[_ b & args] p]
    (make-product b args)))
(defn make-subtraction [a1 a2]
  (cond (or (=number? a1 0) (nil? a1)) (- a2)
        (or (=number? a2 0) (nil? a2)) a1
        (and (number? a1) (number? a2)) (- a1 a2)
        :else (list '- a1 a2)))
(defn base [s] (first s))
(defn exponent [s] (second s))
(defn make-exponentiation [m1 m2]
  (cond (=number? m2 0) 1
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (Math/pow m1 m2)
        :else (list '** m1 m2)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        :else ((get-operation 'deriv (operator exp)) (operands exp)
                                                     var)))
(defn install-deriv-package! []
  (put-operation! 'deriv '+ (fn [exp var]
                              (make-sum (deriv (addend exp) var)
                                        (deriv (augend exp) var))))
  (put-operation! 'deriv '* (fn [exp var]
                              (make-sum
                               (make-product (multiplier exp)
                                             (deriv (multiplicand exp) var))
                               (make-product (deriv (multiplier exp) var)
                                             (multiplicand exp)))))
  (put-operation! 'deriv '** (fn [exp _]
                               (make-product
                                (exponent exp)
                                (make-exponentiation
                                 (base exp)
                                 (make-subtraction (exponent exp) 1))))))

(defn attach-tag [type-tag contents]
  (cons type-tag contents))
(defn make-from-mag-ang [range angle]
  (fn dispatch [op]
    (cond
      (= op 'rectangular)
      (list (* range (Math/cos angle)) (* range (Math/sin angle)))
      (= op 'polar)
      (list range angle)
      :else
      {:error (str "Unknown op -- MAKE-FROM-MAG_ANG" op)})))

(defn make-scheme-number [n]
  ((get-operation 'make 'scheme-number) n))
