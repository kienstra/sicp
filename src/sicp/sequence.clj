(ns sicp.sequence
  (:require [sicp.fib :refer [fib]]))

(defn scale-tree [tree factor]
  (map (fn [sub-tree]
         (if (seq sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))

(defn square [x]
  (* x x))

(defn tree-map [tree term]
  (map (fn [sub-tree]
         (if (list? sub-tree)
           (tree-map sub-tree term)
           (term sub-tree)))
       tree))

(defn square-tree-apply [tree]
  (tree-map tree square))

(defn square-tree-iter [tree final]
  (if (list? (first tree))
    (square-tree-iter (rest tree) final)
    (square (first tree))))

(defn square-tree [tree]
  (square-tree-iter tree '()))

(defn square-tree-higher-order [tree]
  (map (fn [sub-tree]
         (if (list? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))

(defn subsets [s]
  (if (and (seq? s) (empty? s))
    nil
    (let [r (subsets (rest s))]
      (concat r (map #(cons (first s) %) r)))))

(defn pair? [tree]
  (and (list? tree) (= 2 (count tree))))

(defn sum-odd-squares [tree]
  (cond (not tree)
        0
        (not (pair? tree))
        (if (odd? tree) (square tree) 0)
        :else (+ (sum-odd-squares (first tree))
                 (sum-odd-squares (second tree)))))

(defn even-fibs [n]
  ((fn next-fibs [k]
     (if (> k n)
       nil
       (let [f (fib k)]
         (if (even? f)
           (cons f (next-fibs (+ k 1)))
           (next-fibs (+ k 1))))))
   0))

(defn accumulate [op initial sequence]
  (if (and (seq? sequence) (empty? sequence))
    initial
    (op (first sequence)
        (accumulate op initial (rest sequence)))))

(defn map-op [p sequence]
  (accumulate (fn [x y] (p x y)) nil sequence))

(defn append [seq1 seq2]
  (accumulate cons seq1 seq2))

(defn count-leaves [x]
  (cond
    (and (seq? x) (empty? x)) 0
    (not (pair? x)) 1
    :else (+ (count-leaves (first x))
             (count-leaves (second x)))))

(defn count-leaves-accum [t]
  (accumulate + 0
              (map count-leaves t)))

(defn accumulate-n [op init seqs]
  (if (and (seq? (first seqs)) (empty? (first seqs)))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))
