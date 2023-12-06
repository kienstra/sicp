(ns sicp.monte-carlo)

(defn random-in-range [low high]
  (+ low (rand (- high low))))

(defn pi-from-ratio [ratio]
  (* 4 ratio))

(defn estimate-integral-iter [P x1 x2 y1 y2 trials trials-passed]
  (let [x (random-in-range x1 x2)
        y (random-in-range y1 y2)]
    (if
     (= 0 trials)
      trials-passed
      (estimate-integral-iter
       P x1 x2 y1 y2 (dec trials) (if (P x y) (inc trials-passed) trials-passed)))))

(defn estimate-integral [P x1 x2 y1 y2 trials]
  (let [passed (estimate-integral-iter P x1 x2 y1 y2 trials 0)]
    (pi-from-ratio (/ passed trials))))

(defn in-circle? [x y]
  (<= (+ (Math/pow (- x 5) 2) (Math/pow (- y 7) 2)) 9))
