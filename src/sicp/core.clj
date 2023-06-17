(ns sicp.core
  (:require [sicp.generic :refer [install-scheme-number-package!
                                  install-complex-package!
                                  install-rational-package!
                                  make-scheme-number]]))

(defn -main []
  (install-scheme-number-package!)
  (install-complex-package!)
  (install-rational-package!)
  (println (make-scheme-number 4)))
