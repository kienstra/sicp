(ns sicp.ordered-set)

(defn intersection-set [set1 set2]
  (if (or (= '() set1) (= '() set2))
    '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond
        (= x1 x2)
        (cons x1 (intersection-set (rest set1) (rest set2)))
        (< x1 x2)
        (intersection-set (rest set1) set2)
        (< x2 x1)
        (intersection-set set1 (rest set2))))))

(defn element-of-set? [x set]
  (cond (= '() set) false
        (= x (first set)) true
        (< x (first set)) false
        :else (element-of-set? x (rest set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn union-set [set1 set2]
  (cond (= '() set1)
        set2
        (element-of-set? (first set1) set2)
        (union-set (rest set1) set2)
        :else (cons (first set1) (union-set (rest set1) set2))))

(defn union-set-ordered [set1 set2]
  (if (or (= '() set1) (= '() set2))
    '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond
        (= x1 x2)
        (cons x1 (union-set-ordered (rest set1) (rest set2)))
        (< x1 x2)
        (cons x1 (union-set-ordered (rest set1) set2))
        (< x2 x1)
        (cons x2 (union-set-ordered set1 (rest set2)))))))

(defn deduplicate [sequence accum]
  (cond
    (= '() sequence)
    accum
    (element-of-set? (first sequence) (rest sequence))
    (recur (rest sequence) accum)
    :else (recur (rest sequence) (cons (first sequence) accum))))

(defn intersection-set-allow-duplicates [set1 set2]
  (deduplicate (intersection-set set1 set2) '()))

(defn union-set-allow-duplicates [set1 set2]
  (deduplicate (union-set set1 set2) '()))
