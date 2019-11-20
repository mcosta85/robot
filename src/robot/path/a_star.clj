(ns robot.path.a-star
  (:require [clojure.set :refer [difference]]))

(defn manhattan-distance
  "A heuristic algorithm which ignores obstacles, and calculates distance as the
  number of cells in straight line directions to the target. For example, if a cell
  is 4 to the right and 2 up from the current cell, the Manhattan distance is 6.

  Start and end are represented as seqs with two elements, the first being the y axis
  and the second being the x axis from the top left of the world. For example, [1 3]
  is one row down and 3 columns to the right.

  Important Note: Manhattan Distance assumes the World is a cartesian plane."

  [[start-y start-x] [end-y end-x]]
  (* 10
     (+ (Math/abs (- end-y start-y))
        (Math/abs (- end-x start-x)))))

(def ^:private move-costs
  "Default cost for each move a robot can make. In cartesian space, the only
  possible moves are diagonally or up-down-left-right."
  {:diagonal 14
   :straight 10})

(defn straight-move?
  "Returns true if the move was not diagonal."
  [[y1 x1] [y2 x2]]
  (or (= y1 y2) (= x1 x2)))

(defn- weigh-move
  "Given two nodes, determine the cost of a move from start to end."
  [start end]
  (if (= start end)
    0
    (if (straight-move? start end)
      (:straight move-costs)
      (:diagonal move-costs))))

(defn- cheapest-node
  "Given an open map, return the node within the shortest distance to then end."
  [open]
  (reduce (fn [x y]
            (if (< (+ (:from-start x) (:to-end x))
                   (+ (:from-start y) (:to-end y)))
            x y))
          open))

(defn- filter-obstructions
  "Given a seq of nodes, remove any nodes that are obstructed by the world."
  [world nodes]
  (filter #(not= 1 (apply aget world %))
          nodes))

(defn- filter-traversed
  "Given a seq of nodes, remove any nodes that have already been visited."
  [world closed nodes]
  (difference (set nodes)
              (set closed)))

(defn- cartesian-neighbors
  "Returns a seq of the neighbors of a given node on a cartesian plane."
  [world [y x :as node]]
  (let [width (count (get world 1))
        height (count world)]

    (remove #(= % node)
            (for [a (filter #(and (< % width) (nat-int? %)) (range (dec x) (+ 2 x)))
                  b (filter #(and (< % height) (nat-int? %)) (range (dec y) (+ 2 y)))]
              [b a]))))

(defn get-neighbors
  "Returns a seq of all non-traversed neighbor nodes to the current node."
  [world node sense closed]
  (->> (:coords node)
      (sense world)
      (filter-obstructions world)
      (filter-traversed world (map :coords closed))))

(defn- make-node
  ""
  [coords end parent heuristic]
  {:coords coords
   :parent parent
   :from-start (+ (weigh-move (:coords parent coords) coords) (:from-start parent 0))
   :to-end (heuristic end coords)})

(defn merge-open
  "Given an open list and a seq of neighbors, add those neighbors to the open list.
  In the case of a conflict of nodes, retain the cheapest node."
  [o1 o2]
  (reduce-kv (fn [c k v]
               (if (= 1 (count v))
                 (conj c v)
                 (conj c (cheapest-node v))))
             []
             (group-by :coords (into o1 o2))))

(defn- get-path
  "Given the closed list, retrieve the path to the lastmost element."
  [node]
  (loop [path []
         node node]
    (if-not (:parent node)
      (conj path (:coords node))
      (recur (conj path (:coords node)) (:parent node)))))

(defn plan
  "Given a world, starting point, and destination, return the cheapest path from
  start to dest. 'Cheap' is determined by the heuristic function provided.

  world is expected to be a 2d seq of 0's and 1's. A 0 is assumed to be an open
  square, whereas a 1 means the square is blocked and a robot can not move there.

  sense is a function that mimics a robot sensing where it's at in it's environment.
  sense functions should take as parameters the world and a current node.If a sense 
  function isn't provided, a function which determines a position in
  cartesian space is provided.

  The heuristic function defaults to Manhattan distance if no function is
  provided."

  ([world start dest]
   (plan world start dest cartesian-neighbors))

  ([world start dest sense]
   (plan world start dest sense manhattan-distance))
  
  ([world start dest sense heuristic]
   (let [world (to-array-2d world)
         start (make-node start dest nil heuristic)]
     (loop [open []
            closed []
            curr start]
       (let [neighbors (get-neighbors world curr sense closed)
             o (reduce #(conj %1 (make-node %2 dest curr heuristic)) [] neighbors)
             open (flatten (merge-open o open))
             nxt (cheapest-node o)]

         (if (= (:coords curr) dest)
           (reverse (get-path curr))
           (recur (disj (set open) nxt) (conj closed curr) nxt)))))))
