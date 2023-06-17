(ns sicp.generic-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp.generic :refer [add
                                  div
                                  install-complex-package!
                                  install-rational-package!
                                  install-scheme-number-package!
                                  make-complex-from-real-imag
                                  make-rational
                                  make-scheme-number
                                  mul
                                  sub]]))

(deftest generic-test
  (install-complex-package!)
  (install-rational-package!)
  (install-scheme-number-package!)

  (testing "Scheme numbers"
    (is (= 11 (add (make-scheme-number 8) (make-scheme-number 3))))
    (is (= 5 (sub (make-scheme-number 8) (make-scheme-number 3))))
    (is (= 24 (mul (make-scheme-number 8) (make-scheme-number 3))))
    (is (= 4 (div (make-scheme-number 12) (make-scheme-number 3)))))

  (testing "Rational numbers"
    (is (= '(rational (8 3)) (add (make-rational 4 3) (make-rational 4 3))))
    (is (= '(rational (2 3)) (sub (make-rational 4 3) (make-rational 2 3))))
    (is (= '(rational (25 16)) (mul (make-rational 5 4) (make-rational 5 4))))
    (is (= '(rational (1 1)) (div (make-rational 8 3) (make-rational 8 3)))))

    (testing "Complex numbers"
      (is (= '(complex (33 0)) (add (make-complex-from-real-imag 18 0) (make-complex-from-real-imag 15 0))))))
