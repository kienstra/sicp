(ns sicp.core
  (:require [sicp.three-fn :refer [three-fn]]
            [sicp.fixed-point :refer [x-pow-x]]))

(defn -main []
  (println (three-fn 11))
  (println (x-pow-x 1000)))
