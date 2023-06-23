(ns sicp.poly (:require [sicp.generic
                         :refer [apply-generic-coerce
                                 get-operation
                                 put-operation!]]))

(defn variable? [x] (symbol? x))
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))
(defn make-poly [variable term-list]
  (cons variable term-list))
(defn variable [p] (first p))
(defn term-list [p] (rest p))
(defn add-poly [p1 p2] (+ p1 p2)) ;; TODO
(defn mul-poly [p1 p2] (* p1 p2)) ;; TODO

(defn attach-tag [type-tag contents]
  (list type-tag contents))
(defn type-tag [datum]
  (cond
    (int? datum)
    'integer
    :else (first datum)))
(defn tag-poly [p] (attach-tag 'polynomial p))

(defn install-polynomial-package! []
  (put-operation! 'add '(polynomial polynomial)
                  (fn [p1 p2] (tag-poly (add-poly p1 p2))))
  (put-operation! 'mul '(polynomial polynomial)
                  (fn [p1 p2] (tag-poly (mul-poly p1 p2))))
  (put-operation! 'make 'polynomial
                  (fn [var terms] (tag-poly (make-poly var terms))))
  'done)

(defn add [& args] (apply-generic-coerce 'add args))
(defn =zero? [& args] (apply-generic-coerce '=zero? args))
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
