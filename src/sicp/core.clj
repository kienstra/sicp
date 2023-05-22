(ns sicp.core
  (:require [sicp.three-fn :refer [three-fn]]
            [sicp.pascal :refer [amount-in-level]]))

(defn -main []
  (println (three-fn 11))
  (println (amount-in-level 11)))
