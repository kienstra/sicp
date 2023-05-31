(ns sicp.set)

(defn element-of-set? [x set]
  (cond (= '() set) false
        (= x (first set)) true
        :else (element-of-set? x (rest set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn adjoin-set-allow-duplicates [x set]
  (cons x set))

(defn intersection-set [set1 set2]
  (cond (or (= '() set1) (= '() set2))
        '()
        (element-of-set? (first set1) set2)
        (cons (first set1)
              (intersection-set (rest set1) set2))
        :else (intersection-set (rest set1) set2)))

(defn union-set [set1 set2]
  (cond (= '() set1)
        set2
        (element-of-set? (first set1) set2)
        (union-set (rest set1) set2)
        :else (cons (first set1) (union-set (rest set1) set2))))

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
