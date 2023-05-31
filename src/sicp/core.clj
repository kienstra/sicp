(ns sicp.core
  (:require [sicp.tree :refer [element-of-set? make-tree]]))

(defn -main []
  (println (element-of-set? 9 (make-tree 5 1 9))))
