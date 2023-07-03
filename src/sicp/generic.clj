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
    (integer? datum)
    'integer
    (ratio? datum)
    'integer
    (double? datum)
    'double
    :else (first datum)))
(defn contents [datum]
  (cond
    (pair? datum)
    (second datum)
    (number? datum)
    datum
    :else {:error (str "Bad datum -- TYPE-TAG" datum)}))
(defn gcd-num [a b]
  (if (= b 0)
    a
    (gcd-num b (mod a b))))

(defn install-integer-package! []
  (put-operation! 'add '(integer integer) +)
  (put-operation! 'sub '(integer integer) -)
  (put-operation! 'mul '(integer integer) *)
  (put-operation! 'div '(integer integer) /)
  (put-operation! 'greatest-common-divisor '(integer integer) gcd-num)
  (put-operation! 'equ? '(integer integer) =)
  (put-operation! '=zero? '(integer) #(zero? %))
  (put-operation! 'make 'integer identity)
  'done)

(defn make-integer [n]
  ((get-operation 'make 'integer) n))
(defn numer [x] (first x))
(defn denom [x] (second x))
(defn make-rational [n d]
  ((get-operation 'make 'rational) n d))
(defn make-rat-gcd [n d]
  (let [g (gcd-num n d)]
    (list (/ n g) (/ d g))))
(defn add-rat [x y]
  (make-rational (+ (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))
(defn sub-rat [x y]
  (make-rational (- (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))
(defn mul-rat [x y]
  (make-rational (* (numer x) (numer y))
                 (* (denom x) (denom y))))
(defn div-rat [x y]
  (make-rational (* (numer x) (denom y))
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
                  #(add-rat %1 %2))
  (put-operation! 'sub '(rational rational)
                  #(sub-rat %1 %2))
  (put-operation! 'mul '(rational rational)
                  #(mul-rat %1 %2))
  (put-operation! 'div '(rational rational)
                  #(div-rat %1 %2))
  (put-operation! 'equ? '(rational rational)
                  #(equ-rat? %1 %2))
  (put-operation! '=zero? '(rational) #(=zero-rat? %))
  (put-operation! 'make 'rational
                  #(tag-rat (make-rat-gcd %1 %2)))
  'done)

(defn tag-real [x] (attach-tag 'real x))
(defn install-real-package! []
  (put-operation! 'add '(real real) +)
  (put-operation! 'sub '(real real) -)
  (put-operation! 'mul '(real real) *)
  (put-operation! 'div '(real real) /)
  (put-operation! 'greatest-common-divisor '(real real) gcd-num)
  (put-operation! 'equ? '(real real) =)
  (put-operation! '=zero? '(real) #(zero? %))
  (put-operation! 'make 'real #(tag-real %))
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
  (zero? (real-part z)))
(defn tag-complex [z] (attach-tag 'complex z))
(defn make-from-real-imag-polar [x y]
  (list x y))
(defn make-complex-from-mag-ang-polar [x y]
  (list x y))

(defn install-complex-package! []
  (put-operation! 'add '(complex complex)
                  #(add-complex %1 %2))
  (put-operation! 'sub '(complex complex)
                  #(sub-complex %1 %2))
  (put-operation! 'mul '(complex complex)
                  #(mul-complex %1 %2))
  (put-operation! 'div '(complex complex)
                  #(div-complex %1 %2))
  (put-operation! 'equ? '(complex complex)
                  #(equ-complex? %1 %2))
  (put-operation! '=zero? '(complex) #(=zero-complex? %))
  (put-operation! 'make-from-real-imag 'complex
                  #(tag-complex (make-from-real-imag-polar %1 %2)))
  (put-operation! 'make-from-mag-ang 'complex
                  #(tag-complex (make-complex-from-mag-ang-polar %1 %2)))
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
(def gcd #(apply-generic-coerce 'greatest-common-divisor %&))
(def equ? #(apply-generic-coerce 'equ? %&))
(def =zero? #(apply-generic-coerce '=zero? %&))
