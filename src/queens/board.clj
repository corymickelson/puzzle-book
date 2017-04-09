(ns queens.board)

(defn blank-board
  "Create an n x n board, represented as a single array."
  [n]
  (vec (range 0 (inc (Math/pow n 2)))))

(defn rows
  "Bounds defines the ends of the row.
  Ex. for a 4 x 4 board the bounds will be '(0 4 8 12 16)
  Leaving the value of each row a range from indexN to indexN + 1"
  [n]
  (let [bounds (filter (fn [i]
                       (= (mod i n) 0))
                       (blank-board n))]
    (keep-indexed
     (fn [idx v]
       (if (> (inc n) (inc idx))
         (let [row (map inc
                        (range
                         (nth bounds idx)
                         (nth bounds (inc idx))))]
           row)))
     bounds)))

(defn position
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn clear-path?
  "Check all paths for possible intersects."
  [q board]
  (some (partial = q) board))


