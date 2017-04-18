(ns queens.tree
  (:require [clojure.set :refer [union]]
            [queens.board :refer :all]
            [queens.queen :refer :all :as queen])
  (:import [queens.board IQueensBoard]
           [queens.board QueensBoard]))

(definterface ILeaf
  (getValue [])
  (getChildren [])
  (findq [q])
  (expand [q])
  (delete [q])
  (vacant []))

(defrecord Leaf
    [^IQueensBoard value children]
  ILeaf
  (getValue [this] value)
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

(defn- straight-line-sink
  "Follow the first child down till depth of n."
  ([^ITree tree depth]
   (if (= depth 0)
     (:root tree) 
     (straight-line-sink (:root tree) depth 0)))
  ([^ILeaf leaf depth at]
   (if (empty? (:children leaf))
     (throw (ex-info "Can not reach depth, children empty."
                     {:last-node leaf
                      :depth-reached at})))
   (if (= at depth)
     leaf
     (recur (first (:children leaf)) depth (inc at)))))

(defn- floor-level 
  [^ILeaf node level]
  (if (empty? (:children node))
    level
    (map (fn [i]
           (floor-level i (inc level)))
         (:children node))))

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

(defn- candidate-list
  [queen row dark]
  (list (list queen)
        (filter (fn [i]
                  (not (some (partial = i) dark)))
                row)))

(defn candidates 
  "Provided the dimension of the board (n)
  and the position of row[0] queen.
  Return list of root position (row[0] queen) 
  and all possible positions of row++"
  ([n queen]
   (let [intersects (queens-path n queen)
         next-row   (second (rows n))
         next-row-q (queens-on-row next-row intersects)]
     (candidate-list queen next-row next-row-q)))
  ([n queen queens]
   (let [depth (count queens)
         active-queen (queen/queens-path n queen)
         intersects (reduce
                     union
                     #{}
                     (map (fn [i]
                            (queens-path n i))
                          (conj queens queen)
                          ))
         next-row   (nth (rows n) (inc depth))
         next-row-q (queens-on-row next-row intersects)]
     (candidate-list queen next-row next-row-q))))

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

(defn- construct-leaf
  [n queen children]
  (->Leaf
   (->QueensBoard n queen (queen/queens-path n queen)) children))

(defn construct-tree
  "Initialize tree
   Can be initialized with the size of the board (n) 
   and the first position.

   Or... If a section of the board is already filled
   pass an additional vector of positions

   All possible positions available for the next unfulfilled 
   row will be declared as children to the last position 
   in the vector."
  ([n queen]
   (let [candidates-list (candidates n queen)
         tree-root (construct-leaf n queen []) 
         child-nodes (map
                      (fn [i]
                        (construct-leaf n i []))
                      (second candidates-list))]
     (map (fn [item]
            (let [children (flatten
                             (conj
                              (list item)
                              (list (:children tree-root))))]
              (assoc tree-root :children children)))
          child-nodes)))
  
  ([n queen queens]
   (let [candidate-leafs (map (fn [item]
                                (construct-leaf n item []))
                              (flatten (candidates n queen queens)))
         working-node (assoc (first candidate-leafs) :children (rest candidate-leafs)) 
         init-tree (reduce (fn [tree node]
                             (let [leaf-node (->Leaf (->QueensBoard n node (queen/queens-path n node)) [])
                                   child-nodes (if (= (last queens) node)
                                                          (assoc leaf-node :children  (list working-node))
                                                          (list leaf-node))]
                                  (assoc tree :children (list child-nodes))))
                              (construct-leaf n (first queens) [])
                              (rest queens))]
     init-tree)))
