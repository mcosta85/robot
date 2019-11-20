(ns robot.move)

(def ^:private move-costs
  "Default cost for each move a robot can make. In cartesian space, the only
  possible moves are diagonally or up-down-left-right."
  {:diagonal 14
   :straight 10
   :no-move 0})

(defn- direction
  "Returns the direction of the given move. Either diagonal of straight."
  [[y1 x1] [y2 x2]]
  (if (and (= y1 y2) (= x1 x2))
    :no-move
    (if (or (= y1 y2) (= x1 x2))
      :straight
      :diagonal)))

(defn weigh-move
  "Given two nodes, determine the cost of a move from start to end."
  ([start end]
   (weigh-move start end move-costs))

  ([start end cost]
   (get cost (direction start end))))
