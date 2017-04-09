(ns queens.core-test
  (:require [clojure.test :refer :all]
            [queens.queen :refer :all]))

(deftest queens-path-spec
  (testing "Center position"
    (is (= (queens-path 4 6) #{7 1 6 3 2 11 9 5 14 16 10 8})))
  (testing "Corner position"
    (is (= (queens-path 4 13) #{1 5 9 13 14 15 16 10 7 4})))
  (testing "Another corner position"
    (is (= (queens-path 4 1) #{1 2 3 4 5 9 13 6 11 16}))))

