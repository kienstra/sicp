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
  (list type-tag contents))
(defn type-tag [datum]
  (cond
    (int? datum)
    'integer
    :else (first datum)))
(defn contents [datum]
  (cond
    (pair? datum)
    (second datum)
    (number? datum)
    datum
    :else {:error (str "Bad datum -- TYPE-TAG" datum)}))

(defn install-integer-package! []
  (put-operation! 'add '(integer integer) +)
  (put-operation! 'sub '(integer integer)
                  (fn [x y] (- x y)))
  (put-operation! 'mul '(integer integer)
                  (fn [x y] (* x y)))
  (put-operation! 'div '(integer integer)
                  (fn [x y] (/ x y)))
  (put-operation! 'equ? '(integer integer)
                  (fn [x y] (= x y)))
  (put-operation! '=zero? '(integer) zero?)
  (put-operation! 'make 'integer identity)
  'done)

(defn make-integer [n]
  ((get-operation 'make 'integer) n))

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))
(defn numer [x] (first x))
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

(defn tag-real [x] (attach-tag 'real x))
(defn install-real-package! []
  (put-operation! 'add '(real real)
                  (fn [x y] (tag-real (+ x y))))
  (put-operation! 'sub '(real real)
                  (fn [x y] (tag-real (- x y))))
  (put-operation! 'mul '(real real)
                  (fn [x y] (tag-real (* x y))))
  (put-operation! 'div '(real real)
                  (fn [x y] (tag-real (/ x y))))
  (put-operation! 'equ? '(real real)
                  (fn [x y] (= x y)))
  (put-operation! '=zero? '(real) =)
  (put-operation! 'make 'real
                  (fn [n] (tag-real n)))
  'done)
(defn make-real [n]
  ((get-operation 'make 'real) n))

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

(defn error? [e]
  (boolean (get e :error nil)))

(defn raise-next [n from]
  (cond
    (= from 'integer)
    (make-rational n 1)
    (= from 'rational)
    (make-real (double (/ (numer n) (denom n))))
    (= from 'real)
    (make-complex-from-real-imag n 0)
    :else {:error (format "Could not raise %1$s from %2$s"
                          n from)}))

(defn raise [n to]
  (let [from (type-tag n)]
    (if
     (or (= from to) (error? n))
      n
      (recur (raise-next (contents n) from) to))))

(defn drop-next [x]
  (let [cont (contents x)
        type (type-tag x)]
    (cond
      (= type 'complex)
      (make-real (real-part cont))
      (= type 'real)
      (cond
        (int? cont)
        (make-rational cont 1)
        (zero? (- cont (Math/floor cont)))
        (make-rational (int cont) 1)
        :else
        (make-rational (numerator (rationalize cont)) (denominator (rationalize cont))))
      (= type 'rational)
      (if (= 1 (denom cont))
        (make-integer (numer cont))
        {:error (format "Could not lower rational number %1$s to int" cont)})
      :else {:error (format "Could not lower number %1$s of type %2$s"
                            cont type)})))

(defn drop-num [n to equ]
  (let [next-lower (drop-next n)]
    (cond
      (= to (type-tag n))
      n
      (error? next-lower)
      next-lower
      :else (recur next-lower to equ))))

(defn apply-generic-coerce [op args]
  (let [operands (take 2 args)
        remaining (drop 2 args)
        [a1 a2] operands
        type-tags (map type-tag operands)
        proc (get-operation op type-tags)
        [type1 type2] type-tags]
    (cond
      (and proc (seq remaining))
      (recur
       op
       (cons (apply proc (map contents operands))
             remaining))
      proc
      (apply proc (map contents operands))
      (= type1 type2)
      {:error (format "No method for the same types %1$s and %2$s"
                      type1
                      type2)}
      :else
      (let [dropped-a1 (drop-num a1 type2 #(apply-generic-coerce 'equ? (list %1 %2)))
            dropped-a2 (drop-num a2 type1 #(apply-generic-coerce 'equ? (list %1 %2)))]
        (cond
          (not (error? dropped-a1))
          (recur op (cons dropped-a1 (cons a2 remaining)))
          (not (error? dropped-a2))
          (recur op (cons a1 (cons dropped-a2 remaining)))
          :else
          {:error (format "No method for the op %1$s and types %2$s and %3$s"
                          op type1 type2)})))))
(def add #(apply-generic-coerce 'add %&))
(def sub #(apply-generic-coerce 'sub %&))
(def mul #(apply-generic-coerce 'mul %&))
(def div #(apply-generic-coerce 'div %&))
(def equ? #(apply-generic-coerce 'equ? %&))
(def =zero? #(apply-generic-coerce '=zero? %&))
