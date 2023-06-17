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
    (pair? datum)
    (first datum)
    (number? datum)
    'scheme-number
    :else {:error (str "Bad tagged datum -- TYPE-TAG" datum)}))
(defn contents [datum]
  (cond
    (pair? datum)
    (second datum)
    (number? datum)
    datum
    :else {:error (str "Bad datum -- TYPE-TAG" datum)}))
(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (get-operation op type-tags)]
    (if proc
      (apply proc (map contents args))
      {:error
       (str "No method for these types -- APPLY-GENERIC"
            (list op type-tags))})))
(defn tag-scheme-number [x]
  (attach-tag 'scheme-number x))
(defn install-scheme-number-package! []
  (put-operation! 'add '(scheme-number scheme-number)
                  (fn [x y] (tag-scheme-number (+ x y))))
  (put-operation! 'sub '(scheme-number scheme-number)
                  (fn [x y] (tag-scheme-number (- x y))))
  (put-operation! 'mul '(scheme-number scheme-number)
                  (fn [x y] (tag-scheme-number (* x y))))
  (put-operation! 'div '(scheme-number scheme-number)
                  (fn [x y] (tag-scheme-number (/ x y))))
  (put-operation! 'make 'scheme-number
                  (fn [x] (tag-scheme-number x)))
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

  (put-operation! 'make 'rational
                  (fn [n d] (tag-rat (make-rat n d))))
  'done)
(defn make-rational [n d]
  ((get-operation 'make 'rational) n d))

(defn make-from-real-imag [x y]
  ((get-operation 'make-from-real-imag 'rectangular) x y))
(defn make-from-mag-ang [r a]
  ((get-operation 'make-from-mag-ang 'polar) r a))
(defn real-part [z] (first z))
(defn imag-part [z] (second z))
(defn square [x] (* x x))
(defn magnitude [z]
  (Math/sqrt (+ (square (real-part z)) (square (imag-part z)))))
(defn angle [z]
  (Math/atan (real-part z))) ;; used to have a first argument of (imag-part z)
(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
(defn tag-complex [z] (attach-tag 'complex z))

(defn install-complex-package! []
  (put-operation! 'add '(complex complex)
                  (fn [z1 z2] (tag-complex (add-complex z1 z2))))
  (put-operation! 'sub '(complex complex)
                  (fn [z1 z2] (tag-complex (sub-complex z1 z2))))
  (put-operation! 'mul '(complex complex)
                  (fn [z1 z2] (tag-complex (mul-complex z1 z2))))
  (put-operation! 'div '(complex complex)
                  (fn [z1 z2] (tag-complex (div-complex z1 z2))))
  (put-operation! 'make-from-real-imag 'complex
                  (fn [x y] (tag-complex (make-from-real-imag x y))))
  (put-operation! 'make-from-mag-ang 'complex
                  (fn [r a] (tag-complex (make-from-mag-ang r a))))
  'done)

(defn make-complex-from-real-imag [x y]
  ((get 'make-from-real-imag 'complex) x y))
(defn make-complex-from-mag-ang [r a]
  ((get 'make-from-mag-ang 'complex) r a))

(defn add [x y] (apply-generic 'add x y))
(defn sub [x y] (apply-generic 'sub x y))
(defn mul [x y] (apply-generic 'mul x y))
(defn div [x y] (apply-generic 'div x y))
