(ns sicp.fixed-point)

(def tolerance 0.00001)
(defn close-enough? [a b]
  (< (abs (- a b)) tolerance))
(defn try-guess [f guess]
  (let [next (f guess)]
    (if (close-enough? guess next)
      next
      (try-guess f next))))
(defn fixed-point [f first-guess]
  (try-guess f first-guess))

(defn average [& args]
  (/ (reduce + args) (count args)))

(defn sqrt [x]
  (fixed-point #(average % (/ x %)) 1.0))

(defn x-pow-x [y]
  (fixed-point #(/ (Math/log y) (Math/log %)) 10))

(defn average [& args]
  (/ (reduce + args) (count args)))

(defn average-damp [f]
  #(average % (f %)))

(defn square [x]
  (* x x))

(defn sqrt-damp [x]
  (fixed-point (average-damp #(/ x %)) (/ x 2)))

(defn cube-root [x]
  (fixed-point (average-damp #(/ x (square %))) (/ x 5)))

(def dx 0.00001)

(defn deriv [g]
  #(/ (- (g (+ % dx)) (g %)) dx))

(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))

(defn sqrt-transform [x]
  (fixed-point-of-transform #(/ x %) average-damp 1.0))

(defn double [term x]
  (term (term x)))

(defn compose [& args]
  (fn [x] (reduce #(%2 %1) x (reverse args))))

(defn repeated [f n]
  (apply compose (repeat n f)))
