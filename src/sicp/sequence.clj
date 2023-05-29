(ns sicp.sequence)

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
  (if (empty? s)
    (list s)
    (let [r (subsets (rest s))]
      (cons r (map #(cons (first s) %) r)))))

(defn pair? [tree]
  (and (list? tree) (= 2 (count tree))))

(defn sum-odd-squares [tree]
  (cond (not tree) 0
        (not (pair? tree))
             (if (odd? tree) (square tree) 0)
        :else (+ (sum-odd-squares (first tree))
              (sum-odd-squares (second tree)))))
