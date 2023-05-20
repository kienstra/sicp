(ns sicp.count-change)

(defn first-denomination [kinds-of-coins]
  (get
   {1 1
    2 5
    3 10
    4 25
    5 50}
   kinds-of-coins))

(defn cc [amount kinds-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins))))

(defn count-change [amount] (cc amount 5))
