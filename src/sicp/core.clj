(ns sicp.core
  (:require [sicp.three-fn :refer [three-fn]]
            [sicp.rat :refer [make-rat]]))

(defn -main []
  (println (three-fn 11))
  (println (make-rat 1 2)))
