(ns sicp.core
  (:require [sicp.complex :refer [deriv]]))

(defn -main []
  (println (deriv '(* (* x y) (+ x 3)) 'x)))
