(ns sicp.evaluator-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.evaluator :refer [eval-metacircular]]))

(deftest evaluator
  (testing "Eval metacircular"
    (is (= 99 (eval-metacircular 99 '())))
    (is (= "example" (eval-metacircular "example" '())))
    (is (= 45 (eval-metacircular 'x (list (list 'x) (list 45)))))
    (is (= "another" (eval-metacircular 'y (list (list 'x 'y) (list 45 "another")))))
    (is (= "was quoted" (eval-metacircular (list 'quote "was quoted") '())))))
