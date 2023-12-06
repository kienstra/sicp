(ns sicp.evaluator-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.evaluator :refer [eval-metacircular
                                    lookup-variable-value
                                    make-begin]]))

(deftest evaluator
  (testing "Eval metacircular"
    (is (= 11 (lookup-variable-value 'x '(((x y) (11 "something"))))))
    (is (= 99 (eval-metacircular '() (make-begin '(99)))))
    (is (= "example" (eval-metacircular '() (make-begin '("example")))))
    (is (= 45 (eval-metacircular '(((x) (45))) (make-begin '(x)))))
    (is (= "another" (eval-metacircular '(((x y) (45 "another"))) (make-begin '(y)))))
    (is (= "was quoted" (eval-metacircular '() (make-begin '(quote "was quoted")))))
    (is (= 89 (eval-metacircular '(((x) ())) (make-begin '(x (set x 89))))))))
