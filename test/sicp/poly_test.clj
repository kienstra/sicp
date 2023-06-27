(ns sicp.poly-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.poly :refer [add-poly
                               add-terms
                               div-poly
                               install-make-rational-poly!
                               install-polynomial-package!
                               make-poly
                               make-polynomial
                               mul-poly
                               remainder-poly
                               term-list
                               variable]]
            [sicp.generic :refer [apply-generic-coerce
                                  install-complex-package!
                                  install-rational-package!
                                  install-real-package!
                                  install-integer-package!
                                  make-rational
                                  sub
                                  =zero?]]))

(deftest poly-test
  (install-complex-package!)
  (install-polynomial-package!)
  (install-rational-package!)
  (install-real-package!)
  (install-integer-package!)

  (testing "Helpers"
    (is (= '(1 2) (term-list '(x 1 2))))
    (is (= 'x (variable '(x 1 2)))))
  (testing "Make poly"
    (is (= '(x 1 2) (make-poly 'x '(1 2))))
    (is (= '(x (2 1) (0 1)) (make-poly 'x '((2 1) (0 1)))))
    (is (= '(x (3 1) (0 1)) (make-poly 'x '((3 1) (0 1))))))
  (testing "Add terms"
    (is (= '((3 4) (1 2)) (add-terms '((1 2)) '((3 4))))))
  (testing "Add poly"
    (is (= '(x (5 6) (7 8) (1 2) (3 4)) (add-poly (make-poly 'x '((1 2) (3 4))) (make-poly 'x '((5 6) (7 8))))))
    (is (= '(x (2 6) (5 7)) (add-poly (make-poly 'x '((2 3) (5 6))) (make-poly 'x '((2 3) (5 1)))))))
  (testing "Multiply poly"
    (is (= '(x (8 24) (10 32) (6 12) (8 16))
           (mul-poly (make-poly 'x '((1 2) (3 4))) (make-poly 'x '((5 6) (7 8)))))
        (= '(x (3 6) (4 6))
           (mul-poly (make-poly 'x '((3 4) (4 4))) (make-poly 'x '((3 2) (4 2)))))))
  (testing "Divide poly"
    (is (= '(x (3 1) (1 1) (1 1) (0 -1))
           (div-poly (make-poly 'x '((5 1) (0 -1))) (make-poly 'x '((2 1) (0 -1))))))

    (is (= '(x (0 1))
           (div-poly (make-poly 'x '((1 1) (0 1))) (make-poly 'x '((1 1) (0 1)))))))

  (testing "Remainder poly"
    (is (= '((1 1) (0 -1))
           (remainder-poly '(x (3 1) (1 1) (1 1) (0 -1))))))

  (testing "Apply generic"
    (is (= '(polynomial (x (5 6) (7 8) (1 2) (3 4))) (apply-generic-coerce
                                                      'add
                                                      (list (make-polynomial 'x '((1 2) (3 4)))
                                                            (make-polynomial 'x '((5 6) (7 8))))))))
  (testing "Make rational with polynomial"
    (install-make-rational-poly!)
    (is (= '(rational ((polynomial (x (2 1) (0 1))) (polynomial (x (3 1) (0 1)))))
           (make-rational
            (make-polynomial 'x '((2 1) (0 1)))
            (make-polynomial 'x '((3 1) (0 1))))))
    (install-rational-package!))
  (testing "Zero?"
    (is (= false (=zero? (make-polynomial 'x '((2 8) (3 4))))))
    (is (= false (=zero? (make-polynomial 'x '((2 0) (3 4))))))
    (is (= false (=zero? (make-polynomial 'x (list (list 2 (make-rational 5 4)) (list 3 (make-rational 0 9)))))))
    (is (= true (=zero? (make-polynomial 'x (list (list 2 (make-rational 0 4)) (list 3 (make-rational 0 9)))))))
    (is (= true (=zero? (make-polynomial 'x '((2 0) (3 0)))))))

  (testing "Subtraction"
    (is (= '(polynomial (x (2 4) (3 1))) (sub (make-polynomial 'x '((2 8) (3 4))) (make-polynomial 'x '((2 4) (3 3))))))
    (is (= '(polynomial (x)) (sub (make-polynomial 'x '((2 8) (3 8))) (make-polynomial 'x '((2 8) (3 8))))))))
