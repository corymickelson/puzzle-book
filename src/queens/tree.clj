(ns queens.tree
  (:require [clojure.set :refer [union]]
            [queens.board :refer :all]
            [queens.queen :refer :all :as queen])
  (:import [queens.board IQueensBoard]
           [queens.board QueensBoard]))

(definterface ILeaf
  (getValue [])
  (getParent [])
  (getChildren [])
  (findq [q])
  (expand [q])
  (delete [q])
  (vacant []))

(defrecord Leaf
    [^ILeaf parent ^IQueensBoard value children]
  ILeaf
  (getValue [this] value)
  (getParent [this] parent)
  (getChildren [this] children)
  (findq [this q] nil)
  (expand [this q] nil)
  (delete [this q] nil)
  (vacant [this] nil))

(definterface ITree
  (getRoot [])
  (complete [n]))

(defrecord Tree
    [^ILeaf root]
  ITree
  (getRoot [this] root)
  (complete [this n] nil))
(defn- queens-on-row
  "Row is the row in target. 
  Dark is the collective paths of all queens."
  [row dark]
  (filter (fn [i]
            (if (and
                 (>= i (first row))
                 (<= i (last row)))
              i))
          dark))

(defn candidates 
  "Provided the dimension of the board (n)
  and the position of row[0] queen.
  Return list of root position (row[0] queen) 
  and all possible positions of row++"
  ([n queen]
   (let [intersects (queens-path n queen)
         next-row   (second (rows n))
         next-row-q (queens-on-row next-row intersects)]
     (list (list queen) (filter (fn [i]
                              (not (some (partial = i) next-row-q)))
                            next-row))))
  ([n queen queens depth]
   (let [intersects (reduce
                     union
                     #{}
                     (map (fn [i]
                            (queens-path n i))
                          (conj queens queen)))
         next-row   (nth (rows n) (inc depth))
         next-row-q (queens-on-row next-row intersects)]
     (list (list queen) (filter (fn [i]
                              (not (some (partial = i) next-row-q)))
                            next-row)))))

(defn queens-row-n
  "tree '((1) (7 8))
  n = 4"
  [tree n]
  (let [active-row-index (dec (count tree)) ;; (1)
        next-row (nth (rows n) (inc active-row-index))
        candidate-positions (nth tree active-row-index) ;; (7 8)
        paths (map (fn [i]
                    (map (fn [j]
                           (queens-path n j))
                         i))
                   tree)
        boards (map (fn [i]
                      (map (fn [j]
                             (union i j)) ;; unions creating oddly formatted set
                           i))
                    paths)]
    (println "active-row-index: " active-row-index)
    (println "next-row: " next-row)
    (println "candidate-positions: " candidate-positions)
    (println "paths: " paths)))

(defn- leaf-iterator
  "Loop over leaf value until the bottom is reached 
  or the value is found. Returns the Leaf of the found value."
  [leaf q]
  (let [search (filter (fn [i]
                    (if (= (:value i) q)
                      leaf))
                       (:children leaf))]
    (if-not (empty? search)
     search)))

