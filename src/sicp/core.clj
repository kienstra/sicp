(ns sicp.core
  (:require [sicp.huffman :refer [generate-huffman-tree sample-pairs]]))

(defn -main []
  (println (generate-huffman-tree sample-pairs)))
