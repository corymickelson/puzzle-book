(ns queens.tree
  (:require [queens.board :refer :all :as board]
            [queens.queen :refer :all :as queen]))


(deftype Board
    [^ints board
     ^ints intersects
     ^ints queens])

(deftype Leaf [^int value ^ints children])

(defn- build-parent
  [value]
  )
(defn add-leaf
  "parent is a set of positions after recursively
      applying each parent position

  child is the position of the queen."
  [parent child]
  ())
