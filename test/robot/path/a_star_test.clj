(ns robot.path.a-star-test
  (:require [clojure.test :refer [deftest testing is]]
            [robot.path.a-star :as a-star]))

(def cartesian-neighbors #'a-star/cartesian-neighbors)

(deftest manhattan-distance-test
  (testing "The distance between a node and itself is 0"
    (is (= 0 (a-star/manhattan-distance [0 0] [0 0]))))

  (testing "The distance between two non-diagonal neighbor nodes is 1"
    (is (= 1 (a-star/manhattan-distance [0 0] [1 0])))
    (is (= 1 (a-star/manhattan-distance [0 0] [0 1])))
    (is (= 1 (a-star/manhattan-distance [1 0] [0 0])))
    (is (= 1 (a-star/manhattan-distance [0 1] [0 0]))))


  (testing "The distance between two diagonal neighbor nodes is 2"
    (is (= 2 (a-star/manhattan-distance [0 1] [1 0])))
    (is (= 2 (a-star/manhattan-distance [1 0] [0 1]))))

  (testing "The distance between [0 0] and [3 2] is 5"
    (is (= 5 (a-star/manhattan-distance [0 0] [3 2])))))

(deftest cartesian-neighbors-test
  (let [world [[0 0 0]
               [0 1 0]
               [0 1 0]]
        middle (cartesian-neighbors world [1 1])
        corner (cartesian-neighbors world [2 2])
        origin (cartesian-neighbors world [0 0])]

    (testing "Given a 3 x 3 plane and node [1 1], should returns 8 neighbors"
      (is (= 8 (count middle))))
    
    (testing "Given a 3 x 3 plane and node [2 2] should return 3 neighbors"
      (is (= 3 (count corner))))

    (testing "Given a 3 x 3 plane and node [0 0] should return 3 neighbors"
      (is (= 3 (count origin))))

    (testing "Givem a 3 x 3 plane and node [5 5] should return an empty seq"
      (is (empty? (cartesian-neighbors world [5 5]))))))
