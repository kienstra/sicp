(ns sicp.poly (:require [sicp.generic
                         :refer [add
                                 div
                                 gcd
                                 gcd-num
                                 get-operation
                                 mul
                                 put-operation!
                                 tag-rat
                                 =zero?]]))

(defn variable? [x] (symbol? x))
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))
(defn make-poly [variable term-list]
  (cons variable term-list))
(defn variable [p] (first p))
(defn term-list [p] (rest p))
(defn make-rational-poly [n d]
  (list n d))
(defn attach-tag [type-tag contents]
  (list type-tag contents))
(defn type-tag [datum]
  (cond
    (int? datum)
    'integer
    :else (first datum)))
(defn tag-poly [p] (attach-tag 'polynomial p))

(defn make-polynomial [var terms]
  ((get-operation 'make 'polynomial) var terms))
(defn coeff [term] (second term))
(defn adjoin-term [term term-list]
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))
(defn the-empty-termlist [] '())
(defn first-term [term-list] (first term-list))
(defn rest-terms [term-list] (rest term-list))
(defn empty-termlist? [term-list] (empty? term-list))
(defn make-term [order coeff] (list order coeff))
(defn order [term] (first term))
(defn add-terms [L1 L2]
  (cond (empty-termlist? L1)
        L2
        (empty-termlist? L2)
        L1
        :else
        (let [t1 (first-term L1)
              t2 (first-term L2)]
          (cond (> (order t1) (order t2))
                (adjoin-term
                 t1 (add-terms (rest-terms L1) L2))
                (< (order t1) (order t2))
                (adjoin-term
                 t2 (add-terms L1 (rest-terms L2)))
                :else
                (adjoin-term
                 (make-term (order t1)
                            (add (coeff t1) (coeff t2)))
                 (add-terms (rest-terms L1)
                            (rest-terms L2)))))))

(defn add-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (term-list p2)))
    {:error (str "Polys not in same var -- ADD-POLY"
                 (list p1 p2))}))

(defn sub-terms [p1 p2]
  (add-terms p1
             (map #(make-term (order %) (mul -1 (coeff %))) p2)))

(defn sub-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (sub-terms (term-list p1) (term-list p2)))
    {:error (str "Polys not in same var -- ADD-POLY"
                 (list p1 p2))}))

(defn mul-term-by-all-terms [t1 L]
  (if (empty-termlist? L)
    (the-empty-termlist)
    (let [t2 (first-term L)]
      (adjoin-term
       (make-term (+ (order t1) (order t2))
                  (mul (coeff t1) (coeff t2)))
       (mul-term-by-all-terms t1 (rest-terms L))))))

(defn mul-terms [L1 L2]
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
               (mul-terms (rest-terms L1) L2))))
(defn mul-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (mul-terms (term-list p1)
                          (term-list p2)))
    {:error (str "Polys not in same var -- MUL-POLY"
                 (list p1 p2))}))

(defn integerizing-factor [p q]
  (int (Math/pow (coeff (first-term q)) (- (+ 1 (order (first-term p))) (order (first-term q))))))

(defn div-term-coeffs-by-num [L n]
  (if (empty-termlist? L)
    L
    (map #(make-term (order %) (div (coeff %) n)) L)))

(defn div-terms [L1 L2]
  (if (empty-termlist? L1)
    nil
    (let [t1 (first-term L1)
          t2 (first-term L2)]
      (if (> (order t2) (order t1))
        L1
        (let [new-c (div (coeff t1) (coeff t2))
              new-o (- (order t1) (order t2))
              rest-of-result (div-terms
                              (sub-terms
                               L1
                               (mul-terms L2 (list (make-term new-o new-c))))
                              L2)]
          (cons
           (make-term new-o new-c)
           rest-of-result))))))

(defn last-descending-terms [n]
  (reduce
   (fn [acc term]
     (if (and (last acc) (>= (order term) (order (last acc))))
       (list term)
       (concat acc (list term))))
   '()
   n))

(defn remainder-terms [n]
  (let [descending (last-descending-terms n)]
    (if (= descending n)
      nil
      descending)))

(defn div-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly
     (variable p1) (div-terms (term-list p1)
                              (term-list p2)))
    {:error (str "Polys not in same var -- MUL-POLY"
                 (list p1 p2))}))

(defn gcd-terms [a b]
  (if (empty-termlist? b)
    a
    (let [int-factor (integerizing-factor a b)
          remainder (remainder-terms (div-terms (mul-term-by-all-terms
                                                 (make-term 0 int-factor)
                                                 a) b))
          gcd-coeff (reduce gcd (map coeff remainder))]
      (gcd-terms
       b
       (div-term-coeffs-by-num remainder gcd-coeff)))))

(defn gcd-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly
     (variable p1) (gcd-terms
                    (term-list p1)
                    (term-list p2)))
    {:error (str "Polys not in same var -- MUL-POLY"
                 (list p1 p2))}))

(defn reduce-terms [n d]
  (let [greatest-common-divisor (gcd-terms n d)
        int-factor (int (Math/pow (coeff (first-term greatest-common-divisor)) (+ 1 (- (Math/max (order (first-term n)) (order (first-term d))) (order (first-term greatest-common-divisor))))))
        n1 (mul-term-by-all-terms
            (make-term 0 int-factor)
            n)
        d1 (mul-term-by-all-terms
            (make-term 0 int-factor)
            d)
        reduced-n (div-terms n1 greatest-common-divisor)
        reduced-d (div-terms d1 greatest-common-divisor)
        gcd-reduced (reduce gcd-num (map coeff (concat reduced-n reduced-d)))]
    (cons
     (div-terms reduced-n (list (make-term 0 gcd-reduced)))
     (list (div-terms reduced-d (list (make-term 0 gcd-reduced)))))))

(defn equ-poly? [p1 p2]
  (and
   (same-variable? (variable p1) (variable p2))
   (= (term-list p1) (term-list p2))))
(defn =zero-poly? [n]
  (every? #(=zero? (coeff %)) (term-list n)))

(defn install-polynomial-package! []
  (put-operation! 'add '(polynomial polynomial)
                  #(tag-poly (add-poly %1 %2)))
  (put-operation! 'sub '(polynomial polynomial)
                  #(tag-poly (sub-poly %1 %2)))
  (put-operation! 'mul '(polynomial polynomial)
                  #(tag-poly (mul-poly %1 %2)))
  (put-operation! 'div '(polynomial polynomial)
                  #(tag-poly (div-poly %1 %2)))
  (put-operation! 'greatest-common-divisor '(polynomial polynomial)
                  #(tag-poly (gcd-poly %1 %2)))
  (put-operation! 'equ? '(polynomial polynomial)
                  #(equ-poly? %1 %2))
  (put-operation! '=zero? '(polynomial)
                  #(=zero-poly? %))
  (put-operation! 'make 'polynomial
                  #(tag-poly (make-poly %1 %2)))
  'done)

(defn install-make-rational-poly! []
  (put-operation! 'make 'rational
                  #(tag-rat (make-rational-poly %1 %2))))
