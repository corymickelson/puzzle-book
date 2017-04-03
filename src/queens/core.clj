(ns queens.core)

(deftype QPosition [^int x ^int y])

(defrecord Leaf [value children])

(comment

  Queens Board 

  | [[:p :e :p :e]
  |  [:p :p :e :e]
 y|  [:Q :p :p :p]
  |  [:p :p :e :e]] 
  |_______________ 
        x
  )
(defn positions-board
  "Create a board of n x n of [x y] coordinates."
  [n]
  (let [row (vec (range 0 n))]
    (map (fn [i j]
           (vec (map (fn [k] [i k]) j)))
         row
         (map (fn [i] row) row))))

(defn queens-board
  "Create a new n x n board.
   Queens Board is a 2d vector of symbols
      where: 
          :e = an empty position
          :p = position in the path of a queen
          :Q = Queen
  Board positions are zero based."
  [n [x y]]
  (let [row (vec (map (fn [_] :e)
                      (range 0 n)))
        posi-board (map (fn [i j]
                          (vec (map (fn [k] [i k]) j)))
                        row
                        (map (fn [i] row) row)) 
        q-board (map (fn [i]
                     (if (= y i)
                       (assoc row x :Q)
                       row))
                   (range 0 n))]
    (filter (fn [p]
              ())
            posi-board)
    q-board)) 
