(ns sicp.generic)

(def table (ref {}))
(defn get-operation [op type]
  (get (get (deref table) op {}) type))
(defn put-operation! [op type item]
  (dosync
   (alter table (fn [previous-table]
                  (into previous-table
                        {op (into (get previous-table op {})
                                  {type item})})))))
(defn pair? [tree]
  (and (list? tree) (= 2 (count tree))))
(defn attach-tag [type-tag contents]
  (if (number? contents)
    contents
    (list type-tag contents)))
(defn type-tag [datum]
  (cond
    (number? datum)
    'scheme-number
    :else (first datum)))
(defn contents [datum]
  (cond
    (pair? datum)
    (second datum)
    (number? datum)
    datum
    :else {:error (str "Bad datum -- TYPE-TAG" datum)}))
(defn apply-generic-naive [op & args]
  (let [type-tags (map type-tag args)
        proc (get-operation op type-tags)]
    (if proc
      (apply proc (map contents args))
      {:error
       (str "No method for these types -- APPLY-GENERIC"
            (list op type-tags))})))
(defn install-scheme-number-package! []
  (put-operation! 'add '(scheme-number scheme-number) +)
  (put-operation! 'sub '(scheme-number scheme-number)
                  (fn [x y] (- x y)))
  (put-operation! 'mul '(scheme-number scheme-number)
                  (fn [x y] (* x y)))
  (put-operation! 'div '(scheme-number scheme-number)
                  (fn [x y] (/ x y)))
  (put-operation! 'equ? '(scheme-number scheme-number)
                  (fn [x y] (= x y)))
  (put-operation! '=zero? '(scheme-number) zero?)
  (put-operation! 'make 'scheme-number identity)
  'done)

(defn make-scheme-number [n]
  ((get-operation 'make 'scheme-number) n))

(defn numer [x] (first x))

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))
(defn denom [x] (second x))
(defn make-rat [n d]
  (let [g (gcd n d)]
    (list (/ n g) (/ d g))))
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

(defn equ-rat? [x y]
  (and
   (= (numer x) (numer y))
   (= (denom x) (denom y))))

(defn =zero-rat? [x]
  (= 0 (numer x)))

(defn tag-rat [x] (attach-tag 'rational x))
(defn install-rational-package! []
  (put-operation! 'add '(rational rational)
                  (fn [x y] (tag-rat (add-rat x y))))
  (put-operation! 'sub '(rational rational)
                  (fn [x y] (tag-rat (sub-rat x y))))
  (put-operation! 'mul '(rational rational)
                  (fn [x y] (tag-rat (mul-rat x y))))
  (put-operation! 'div '(rational rational)
                  (fn [x y] (tag-rat (div-rat x y))))
  (put-operation! 'equ? '(rational rational)
                  (fn [x y] (equ-rat? x y)))
  (put-operation! '=zero? '(rational) =zero-rat?)
  (put-operation! 'make 'rational
                  (fn [n d] (tag-rat (make-rat n d))))
  'done)
(defn make-rational [n d]
  ((get-operation 'make 'rational) n d))

(defn make-complex-from-real-imag [x y]
  ((get-operation 'make-from-real-imag 'complex) x y))
(defn make-complex-from-mag-ang [r a]
  ((get-operation 'make-from-mag-ang 'complex) r a))
(defn real-part [z] (first z))
(defn imag-part [z] (second z))
(defn square [x] (* x x))
(defn magnitude [z]
  (Math/sqrt (+ (square (real-part z)) (square (imag-part z)))))
(defn angle [z]
  (Math/atan (real-part z))) ;; used to have a first argument of (imag-part z)
(defn add-complex [z1 z2]
  (make-complex-from-real-imag (+ (real-part z1) (real-part z2))
                               (+ (imag-part z1) (imag-part z2))))
(defn sub-complex [z1 z2]
  (make-complex-from-real-imag (- (real-part z1) (real-part z2))
                               (- (imag-part z1) (imag-part z2))))
(defn mul-complex [z1 z2]
  (make-complex-from-mag-ang (* (magnitude z1) (magnitude z2))
                             (+ (angle z1) (angle z2))))
(defn div-complex [z1 z2]
  (make-complex-from-mag-ang (/ (magnitude z1) (magnitude z2))
                             (- (angle z1) (angle z2))))
(defn equ-complex? [z1 z2]
  (and (= (real-part z1) (real-part z2))
       (= (imag-part z1) (imag-part z2))))
(defn =zero-complex? [z]
  (= 0 (real-part z)))
(defn tag-complex [z] (attach-tag 'complex z))
(defn make-from-real-imag-polar [x y]
  (list x y))
(defn make-complex-from-mag-ang-polar [x y]
  (list x y))

(defn install-complex-package! []
  (put-operation! 'add '(complex complex)
                  (fn [z1 z2] (add-complex z1 z2)))
  (put-operation! 'sub '(complex complex)
                  (fn [z1 z2] (sub-complex z1 z2)))
  (put-operation! 'mul '(complex complex)
                  (fn [z1 z2] (mul-complex z1 z2)))
  (put-operation! 'div '(complex complex)
                  (fn [z1 z2] (div-complex z1 z2)))
  (put-operation! 'equ? '(complex complex)
                  (fn [z1 z2] (equ-complex? z1 z2)))
  (put-operation! '=zero? '(complex) =zero-complex?)
  (put-operation! 'make-from-real-imag 'complex
                  (fn [x y] (tag-complex (make-from-real-imag-polar x y))))
  (put-operation! 'make-from-mag-ang 'complex
                  (fn [r a] (tag-complex (make-complex-from-mag-ang-polar r a))))
  'done)

(def coercion-table (ref {}))
(defn get-coercion [op type]
  (get (get (deref coercion-table) op {}) type))
(defn put-coercion! [type1 type2 coercion]
  (dosync
   (alter coercion-table (fn [previous-table]
                           (into previous-table
                                 {type1 (into (get previous-table type1 {})
                                              {type2 coercion})})))))

(defn scheme-number->complex [n]
  (make-complex-from-real-imag (contents n) 0))

(defn apply-generic-coerce [op args]
  (let [operands (take 2 args)
        remaining (drop 2 args)
        [a1 a2] operands
        type-tags (map type-tag operands)
        proc (get-operation op type-tags)
        [type1 type2] type-tags]
    (cond
      (and proc (seq remaining))
      (apply-generic-coerce
       op
       (cons (apply proc (map contents operands))
               remaining))
      proc
      (apply proc (map contents operands))
      (not (nil? a2))
      (let [t1->t2 (get-coercion type1 type2)
            t2->t1 (get-coercion type2 type1)]
        (cond
          (= type1 type2)
          {:error (str "No method for these same types"
                       type1
                       type2)}
          t1->t2
          (apply-generic-coerce op (list (t1->t2 a1) a2))
          t2->t1
          (apply-generic-coerce op (list a1 (t2->t1 a2)))
          :else
          {:error (str "No method for these types"
                       (list op type-tags))}))
      :else {:error (str "No method for these types"
                         (list op type-tags))})))

(defn add [& args]
  (apply-generic-coerce 'add args))
(defn sub [& args] (apply-generic-coerce 'sub args))
(defn mul [& args] (apply-generic-coerce 'mul args))
(defn div [& args] (apply-generic-coerce 'div args))
(defn equ? [& args ] (apply-generic-coerce 'equ? args))
(defn =zero? [& args] (apply-generic-coerce '=zero? args))
