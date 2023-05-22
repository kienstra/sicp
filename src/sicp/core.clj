(ns sicp.core
  (:require [sicp.three-fn :refer [three-fn]]
            [sicp.prime :refer [smallest-divisor]]))

(defn -main []
  (println (three-fn 11))
  (println (smallest-divisor 11)))
