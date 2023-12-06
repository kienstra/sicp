(ns sicp.evaluator-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.evaluator :refer [->evaluator
                                    eval-metacircular
                                    make-begin]]))

(deftest evaluator
  (testing "Eval metacircular"
    (is (= 99 (eval-metacircular (->evaluator '()) (make-begin '(99)))))
    (is (= "example" (eval-metacircular (->evaluator '()) (make-begin '("example")))))
    (is (= 45 (eval-metacircular (->evaluator '(((x) (45)))) (make-begin '(x)))))
    (is (= "another" (eval-metacircular (->evaluator '(((x y) (45 "another")))) (make-begin '(y)))))
    (is (= "was quoted" (eval-metacircular (->evaluator '()) (make-begin '(quote "was quoted")))))
    (is (= 89 (eval-metacircular (->evaluator '(((x) ()))) (make-begin '(x (set (x 89)))))))))
