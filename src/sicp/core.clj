(ns sicp.core
  (:require [sicp.count-change :refer [count-change]]))

(defn -main []
  (println (count-change 30)))
