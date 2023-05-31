(ns sicp.tree)

(defn entry [tree] (if (seq? tree) (first (first tree)) tree))
(defn entry-val [tree] (second (first tree)))
(defn left-branch [tree] (if (seq? tree) (nth tree 1) nil))
(defn right-branch [tree] (if (seq? tree) (nth tree 2) nil))
(defn make-tree [entry left right]
  (list entry left right))

; Recursively creates the root, left tree, and right tree.
; The first time it's called, its n will be the length of the list to convert.
; That means it'll convert the entire list.
; This recursively creates the left and right tree with a subset of the list, and their respecitve length.
; > (partial-tree '(1 3 5 7 9 11) 6)
; > (5 (1 nil (3 nil nil)) (9 (7 nil nil) (11 nil nil)))
;
;        5
;   1        9
; 3   7    11
(defn partial-tree [elts n] ; '(1 3 5 7 9 11) 6
  (if (= n 0)
    (cons '() elts)
    (let [left-size (int (/ (- n 1) 2)) ; 2
          left-result (partial-tree elts left-size) ; ((1 nil (3 nil nil)) 5 7 9 11)
          left-tree (first left-result) ; (1 nil (3 nil nil))
          non-left-elts (rest left-result) ; (5 7 9 11)
          right-size (- n (+ left-size 1)) ; 3
          this-entry (first non-left-elts) ; 5
          right-result (partial-tree (rest non-left-elts) ; ((9 (7 nil nil) (11 nil nil)))
                                     right-size)
          right-tree (first right-result) ; (9 (7 nil nil) (11 nil nil))
          remaining-elts (rest right-result)] ; ()
      (cons (make-tree this-entry left-tree right-tree)
            remaining-elts))))

(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

(defn lookup [given-key set-of-records]
  (cond (nil? set-of-records) false
        (= given-key (key (first set-of-records)))
        (first set-of-records)
        :else (lookup given-key (rest set-of-records))))

(defn lookup-bin-tree [given-key set-of-records]
  (cond (= '() set-of-records) false
        (= given-key (entry set-of-records)) (entry-val set-of-records)
        (< given-key (entry set-of-records)) (lookup-bin-tree given-key (left-branch set-of-records))
        (> given-key (entry set-of-records)) (lookup-bin-tree given-key (right-branch set-of-records))
        :else false))
