(ns robot.heuristic)

(def default-weight 10)

(defn manhattan-distance
  "A heuristic algorithm which ignores obstacles, and calculates distance as the
  number of cells in straight line directions to the target. For example, if a cell
  is 4 to the right and 2 up from the current cell, the Manhattan distance is 6.

  Start and end are represented as seqs with two elements, the first being the y axis
  and the second being the x axis from the top left of the world. For example, [1 3]
  is one row down and 3 columns to the right.

  Important Note: Manhattan Distance assumes the World is a cartesian plane."

  ([start end]
   (manhattan-distance start end default-weight))
  
  ([[start-y start-x] [end-y end-x] weight]
   (* weight
      (+ (Math/abs (- end-y start-y))
         (Math/abs (- end-x start-x))))))
