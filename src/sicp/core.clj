(ns sicp.core
  (:require [sicp.three-fn :refer [three-fn]]
            [sicp.sequence :refer [square-tree]]))

(defn -main []
  (println (three-fn 11)))
