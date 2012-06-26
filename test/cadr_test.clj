(ns cadr.test
  (:use [cadr])
  (:use [clojure.test]))

(let [list '(1 2 3 4 5)]
  (deftest test-cadr
    (is (= 2 (cadr list)))))
