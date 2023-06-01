(ns sicp.huffman)

(defn left-branch [tree] (if (seq? tree) (first tree) nil))
(defn right-branch [tree] (if (seq? tree) (second tree) nil))
(defn leaf? [object]
  (and (seq? object) (= (first object) 'leaf)))
(defn symbol-leaf [x] (nth x 1))
(defn weight-leaf [x] (nth x 2))
(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (nth tree 2)))
(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (nth tree 3)))

(defn make-leaf [symbol weight]
  (list 'leaf symbol weight))

(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn make-weighted-leaf [leaf-set]
  (let [left (first (first leaf-set))
        right (second (first leaf-set))]
    (list
     (concat (symbols left) (symbols right))
     (+ (weight left) (weight right)))))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else {:error (str "bad bit -- CHOOSE-BRANCH " bit)}))

(defn decode [bits tree]
  ((fn decode-1 [bits current-branch]
     (if (= '() bits)
       '()
       (let [next-branch
             (choose-branch (first bits) current-branch)]
         (if (leaf? next-branch)
           (cons (symbol-leaf next-branch)
                 (decode-1 (rest bits) tree))
           (decode-1 (rest bits) next-branch)))))
   bits tree))

(defn encode-symbol [message tree]
  ((fn encode-symbol-iter [letters branch bits]
     (if
      (= '() letters)
       bits
       (let [letter (first letters)
             left (left-branch branch)
             right (right-branch branch)]
         (cond
           (and (leaf? left) (= (symbol-leaf left) letter))
           (encode-symbol-iter (rest letters) tree (concat bits '(0)))
           (some #{letter} (symbols left))
           (encode-symbol-iter letters left (concat bits '(0)))
           (and (leaf? right) (= (symbol-leaf right) letter))
           (encode-symbol-iter (rest letters) tree (concat bits '(1)))
           (some #{letter} (symbols right))
           (encode-symbol-iter letters right (concat bits '(1)))
           :else {:error (str "Could not find " letter)}))))
   message tree '()))

(defn encode [message tree]
  (if (nil? message)
    '()
    (concat (encode-symbol (first message) tree)
            (encode (rest message) tree))))

(defn adjoin-set [x set]
  (cond (= '() set) (list x)
        (< (weight x) (weight (first set))) (cons x set)
        :else (cons (first set)
                    (adjoin-set x (rest set)))))

(defn make-leaf-set [pairs]
  (if (= '() pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair) ; symbol
                             (second pair)) ; frequency
                  (make-leaf-set (rest pairs))))))

(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(def sample-pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

(defn successive-merge [leaf-set]
  (if (= 1 (count leaf-set))
    (make-weighted-leaf leaf-set)
    (let [[first second & remaining] leaf-set]
      (successive-merge
       (cons
        (make-code-tree first second)
        remaining)))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))
