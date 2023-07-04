(ns sicp.monte-carlo)

(defn random-in-range [low high]
  (+ low (rand (- high low))))
