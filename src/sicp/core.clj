(ns sicp.core
  (:require [sicp.three-fn :refer [three-fn]]
            [sicp.sum :refer [sum-iter]]))

(defn -main []
  (println (three-fn 11))
  (println (sum-iter identity 0 inc 10)))
