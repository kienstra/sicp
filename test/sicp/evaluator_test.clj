(ns sicp.evaluator-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.evaluator :refer [eval-metacircular]]))

(deftest evaluator
  (testing "Eval metacircular"
    (is (= 99 (eval-metacircular 99 '())))
    (is (= "example" (eval-metacircular "example" '())))
    (is (= 45 (eval-metacircular 'x '((x) (45)))))
    (is (= "another" (eval-metacircular 'y '((x y) (45 "another")))))
    (is (= "was quoted" (eval-metacircular (list 'quote "was quoted") '())))
    (is (= 89 (eval-metacircular ('set 'x 89) '(('x) (45)))))))
