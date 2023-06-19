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
                                  put-coercion!
                                  rational->scheme-number
                                  scheme-number->complex
                                  sub
                                  =zero?]]))

(deftest generic-test
  (install-complex-package!)
  (install-rational-package!)
  (install-scheme-number-package!)

  (testing "Scheme numbers"
    (is (= 11 (add 8 3)))
    (is (= 5 (sub 8 3)))
    (is (= 11 (add (make-scheme-number 8) (make-scheme-number 3))))
    (is (= 5 (sub (make-scheme-number 8) (make-scheme-number 3))))
    (is (= 24 (mul (make-scheme-number 8) (make-scheme-number 3))))
    (is (= 4 (div (make-scheme-number 12) (make-scheme-number 3))))
    (is (= false (=zero? (make-scheme-number 8))))
    (is (= true (=zero? (make-scheme-number 0)))))

  (testing "Multiple scheme numbers"
    (is (= 14 (add 9 3 2)))
    (is (= 4 (sub 9 3 2)))
    (is (= 54 (mul 9 3 2)))
    (is (= 5 (div 30 3 2))))

  (testing "Rational numbers"
    (is (= '(rational (8 3)) (add (make-rational 4 3) (make-rational 4 3))))
    (is (= '(rational (2 3)) (sub (make-rational 4 3) (make-rational 2 3))))
    (is (= '(rational (25 16)) (mul (make-rational 5 4) (make-rational 5 4))))
    (is (= '(rational (1 1)) (div (make-rational 8 3) (make-rational 8 3)))))

  (testing "Multiple rational numbers"
    (is (= '(rational (13 3)) (add (make-rational 4 3) (make-rational 4 3) (make-rational 5 3))))
    (is (= '(rational (1 3)) (sub (make-rational 4 3) (make-rational 2 3) (make-rational 1 3))))
    (is (= '(rational (75 32)) (mul (make-rational 5 4) (make-rational 5 4) (make-rational 3 2))))
    (is (= '(rational (32 3)) (div (make-rational 8 3) (make-rational 1  2) (make-rational 1  2)))))

  (testing "Complex numbers"
    (is (= '(complex (33 0)) (add (make-complex-from-real-imag 18 0) (make-complex-from-real-imag 15 0))))
    (is (= '(complex (3 0)) (sub (make-complex-from-real-imag 18 0) (make-complex-from-real-imag 15 0))))
    (is (= '(complex (2.0000000000000004 1.5707963267948966)) (mul (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 1))))
    (is (= '(complex (7.071067811865476 2.356194490192345)) (mul (make-complex-from-real-imag 3 1) (make-complex-from-real-imag 2 1)))))

  (testing "Multiple complex numbers"
    (is (= '(complex (35 0)) (add (make-complex-from-real-imag 18 0) (make-complex-from-real-imag 15 0) (make-complex-from-real-imag 2 0))))
    (is (= '(complex (1 0)) (sub (make-complex-from-real-imag 18 0) (make-complex-from-real-imag 15 0) (make-complex-from-real-imag 2 0))))
    (is (= '(complex (3.596498602883739 1.892546881191539)) (mul (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 1) (make-complex-from-real-imag 1 1)))))

  (testing "Coercion"
    (put-coercion! 'scheme-number 'complex scheme-number->complex)
    (put-coercion! 'rational 'scheme-number rational->scheme-number)

    (is (= '(complex (40 0)) (add 39 (make-complex-from-real-imag 1 0))))
    (is (= '(complex (40 0)) (add (make-complex-from-real-imag 1 0) 39)))
    (is (= 81/2 (add (make-rational 3 2) 39))))

  (testing "Coercion of multiple numbers"
    (put-coercion! 'scheme-number 'complex scheme-number->complex)
    (put-coercion! 'rational 'scheme-number rational->scheme-number)

    (is (= '(complex (42 0)) (add 39 (make-complex-from-real-imag 1 0) (make-complex-from-real-imag 2 0))))
    (is (= '(complex (47 0)) (add (make-complex-from-real-imag 1 0) 3 (make-complex-from-real-imag 39 0) (make-complex-from-real-imag 4 0))))
    (is (= '(complex (41 0)) (add 39 1 (make-complex-from-real-imag 1 0))))
    (is (= 43 (add (make-rational 3 2) (make-rational 5 2) 39)))))
