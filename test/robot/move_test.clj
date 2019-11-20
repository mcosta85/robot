(ns robot.move-test
  (:require [clojure.test :refer [deftest testing is]]
            [robot.move :as move]))

(def ^:private direction #'move/direction)

(deftest direction-test
  (testing "A move along the x axis xor y axis should yield :straight"
    (is (= :straight (direction [0 0] [0 1])))
    (is (= :straight (direction [1 0] [0 0])))
    (is (= :straight (direction [0 0] [0 5])))
    (is (= :straight (direction [5 0] [0 0]))))


  (testing "A move along both the x and y axes should yield :diagonal"
    (is (= :diagonal (direction [1 0] [0 1])))
    (is (= :diagonal (direction [0 1] [1 0])))
    (is (= :diagonal (direction [1 0] [0 10])))
    (is (= :diagonal (direction [10 0] [0 5]))))

  (testing "A move that starts and ends at the same place should yield :no-move"
    (is (= :no-move (direction [0 0] [0 0])))))


(deftest weigh-move-test
  (testing "Default weights"

    (testing "A diagonal move should cost 14"
      (is (= 14 (move/weigh-move [1 0] [0 1])))
      (is (= 14 (move/weigh-move [0 1] [1 0])))
      (is (= 14 (move/weigh-move [1 0] [0 10])))
      (is (= 14 (move/weigh-move [10 0] [0 5]))))

    (testing "A straight move should cost 10"
      (is (= 10 (move/weigh-move [0 0] [0 1])))
      (is (= 10 (move/weigh-move [0 1] [0 0])))
      (is (= 10 (move/weigh-move [0 0] [10 0])))
      (is (= 10 (move/weigh-move [10 0] [0 0]))))

    (testing "A non-move should cost 0"
      (is (= 0 (move/weigh-move [0 0] [0 0])))))

  (testing "Custom weights"
    (let [weights {:diagonal 20
                   :straight 5
                   :no-move 2}]

      (testing "A diagonal move should cost 20"
        (is (= 20 (move/weigh-move [1 0] [0 1] weights)))
        (is (= 20 (move/weigh-move [0 1] [1 0] weights)))
        (is (= 20 (move/weigh-move [1 0] [0 10] weights)))
        (is (= 20 (move/weigh-move [10 0] [0 5] weights))))

      (testing "A straight move should cost 5"
        (is (= 5 (move/weigh-move [0 0] [0 1] weights)))
        (is (= 5 (move/weigh-move [0 1] [0 0] weights)))
        (is (= 5 (move/weigh-move [0 0] [10 0] weights)))
        (is (= 5 (move/weigh-move [10 0] [0 0] weights))))

      (testing "A non-move should cost 2"
        (is (= 2 (move/weigh-move [0 0] [0 0] weights)))))))
