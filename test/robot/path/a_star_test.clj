(ns robot.path.a-star-test
  (:require [clojure.test :refer [deftest testing is]]
            [robot.path.a-star :as a-star]))

(def cheapest-node #'a-star/cheapest-node)
(def filter-obstructions #'a-star/filter-obstructions)
(def cartesian-neighbors #'a-star/cartesian-neighbors)

(deftest cheapest-node-test
  (testing "Given nodes with scores of 10, 12, and 14, should return the 10 node"
    (let [cheapest {:from-start 5 :to-end 5}
          nodes [cheapest
                 {:from-start 10 :to-end 2}
                 {:from-start 7 :to-end 7}]]
      (is (= cheapest (cheapest-node nodes)))))

  (testing "Given nodes equal scores, should return the last node in seq"
    (let [expected {:from-start 5 :to-end 5 :name "foo"}
          nodes [{:from-start 5 :to-end 5 :name "bar"}
                 expected]
          actual (cheapest-node nodes)]
      (is (= "foo" (:name actual))))))

(deftest filter-obstructions-test
  (testing "Given a world with no obstructions, should return all nodes"
    (let [nodes [[0 0] [0 1] [1 0]]
          world (to-array-2d [[0 0]
                              [0 0]])]
      (is (= nodes (filter-obstructions world nodes)))))

  (testing "Given a world with only obstruction, should return empty seq"
    (let [nodes [[0 0] [0 1] [1 0]]
          world (to-array-2d [[1 1]
                              [1 ]])]
      (is (empty? (filter-obstructions world nodes)))))

  (testing "Given a world with some obstruction, should return a subset of nodes"
    (let [nodes [[0 0] [0 1] [1 0]]
          world (to-array-2d [[1 0]
                              [0 0]])]
      (is (= 2 (count (filter-obstructions world nodes)))))))

(deftest get-neighbors-test
  (testing "Given a 2x2 world with no obstructions and no closed nodes, should
 return 3 nodes."
    (let [world (to-array-2d [[0 0]
                              [0 0]])]
      (is (= 3 (count (a-star/get-neighbors world
                                            {:coords [0 0]}
                                            cartesian-neighbors
                                            [])))))))

(deftest merge-open-test
  (testing "Given two open list with different nodes, should return the union of the two"
    (let [o1 [{:coords [1 1] :from-start 2 :to-end 5}]
          o2 [{:coords [1 2] :from-start 5 :to-end 10}]
          merged (a-star/merge-open o1 o2)]
      (= 2 (count merged))
      (= [[1 1] [1 2]] (map :coords merged))))

  (testing "Given two lists with the same node, should return the node with lower score"
    (let [o1 [{:coords [1 2] :from-start 8 :to-end 5 :name "foo"}]
          o2 [{:coords [1 2] :from-start 5 :to-end 10 :name "bar"}]
          merged (a-star/merge-open o1 o2)]
      (= 1 (count merged))
      (= (= ["foo"] (map :name merged ))))))

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

(deftest plan-test
  (testing "a 2x2 world, should return [0 0] as path"
    (let [world [[0 0]
                 [0 0]]]
      (a-star/plan world [0 0] [1 1]))))
