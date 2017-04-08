(ns queens.core-test
  (:require [clojure.test :refer :all]
            [queens.core :refer :all]))

(deftest generate-positions-board-spec
  (testing "Generate a positions board."
    (is (= [0 0] (first (first (positions-board 4)))))))

(deftest queens-board-spec
  (testing "Queens board q out of bounds handler"
    (is (=  (queens-board 4)))))
