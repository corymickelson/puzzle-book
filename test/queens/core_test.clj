(ns queens.core-test
  (:require [clojure.test :refer :all]
            [queens.core :refer :all]))

(deftest generate-positions-board
  (testing "Generate a positions board."
    (is (= [0 0] (first (first (positions-board 4)))))))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
