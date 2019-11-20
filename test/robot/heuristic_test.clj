(ns robot.heuristic-test
  (:require [clojure.test :refer [deftest testing is]]
            [robot.heuristic :refer [manhattan-distance]]))

(deftest manhattan-distance-test
  (testing "The distance between a node and itself is 0"
    (is (= 0 (manhattan-distance [0 0] [0 0] 1))))

  (testing "The distance between two non-diagonal neighbor nodes is 1"
    (is (= 1 (manhattan-distance [0 0] [1 0] 1)))
    (is (= 1 (manhattan-distance [0 0] [0 1] 1)))
    (is (= 1 (manhattan-distance [1 0] [0 0] 1)))
    (is (= 1 (manhattan-distance [0 1] [0 0] 1))))


  (testing "The distance between two diagonal neighbor nodes is 2"
    (is (= 2 (manhattan-distance [0 1] [1 0] 1)))
    (is (= 2 (manhattan-distance [1 0] [0 1] 1))))

  (testing "The distance between [0 0] and [3 2] is 5"
    (is (= 5 (manhattan-distance [0 0] [3 2] 1)))))
