(ns sicp.core
  (:require [sicp.symbolic-data :refer [deriv]]))

(defn -main []
  (println (deriv 3 '(11 4 24))))
