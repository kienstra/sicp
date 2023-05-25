(ns sicp.pascal)

(defn iter-edges-of-level [count level n]
  (if (> (+ count (+ 1 level)) n)
    [count (+ count level)]
    (iter-edges-of-level (+ count (+ 1 level)) (+ 1 level) n)))

(defn edges-of-level [n]
  (iter-edges-of-level 0 0 n))

(defn edge? [n]
  (contains? (set (edges-of-level n)) n))

(defn left-above [n]
  (let [[left-edge right-edge] (edges-of-level n)
        right-offset (- right-edge n)]
    (- left-edge right-offset 1)))

(defn right-above [n]
  (let [[left-edge right-edge] (edges-of-level n)
        right-offset (- right-edge n)]
    (- left-edge right-offset)))

(defn element [n]
  (if (edge? n)
    1
    (+ (element (left-above n)) (element (right-above n)))))

(defn triangle [n]
  (map element (range n)))
