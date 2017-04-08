(ns queens.core)

(defn- board
  "Think of this as the percolate problem.
  create an area with the area index representing the
  position on the board, and the value as that tiles value."
  [n]
  (vec (range 0 (inc (Math/pow n 2)))))

(defn- path-offset
  [f v]
  (partial f v))

(defn position
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

 (defn- diagonal-
  "Split path at queen. 
  Iterate over left split -1, right split +1"
  [n q path]
  (let [[lower upper] (split-at
                       (first (position (partial = q) path))
                              path)
        lower-diagonal (map-indexed
                        (fn [idx v]
                          (let [abs-value (+ v (inc idx))]
                            (if (>= abs-value 0)
                              abs-value)))
                        lower)
        upper-diagonal (map-indexed
                        (fn [idx v]
                          (let [abs-value (- v (inc idx))]
                            (if (<= abs-value (Math/pow n 2))
                              abs-value)))
                        (rest upper))]
    (filter (fn [i]
              (not (nil? i)))
            (concat lower-diagonal (list q) upper-diagonal))))

(defn- diagonal+
  "Split path at queen. 
  Iterate over left split -1, right split +1"
  [n q path]
  (let [[lower upper] (split-at
                       (first (position (partial = q) path))
                              path)
        lower-diagonal (map-indexed
                        (fn [idx v]
                          (let [abs-value (- v (inc idx))]
                            (if (>= abs-value 0)
                              abs-value)))
                        lower)
        upper-diagonal (map-indexed
                        (fn [idx v]
                          (let [abs-value (+ v (inc idx))]
                            (if (<= abs-value (Math/pow n 2))
                              abs-value)))
                        (rest upper))]
    (filter (fn [i]
              (not (nil? i)))
            (concat lower-diagonal (list q) upper-diagonal))))

(defn- horizontal
  [n q]
  (let [rows (filter (fn [i]
                       (= (mod i n) 0))
                     (board n))]
    (keep-indexed (fn [idx v]
                    (if (> n (inc idx))
                      (let [row (map inc
                                     (range
                                      (nth rows idx)
                                      (nth rows (inc idx))))]
                        (if (some (partial = q) row)
                          row))))
                  rows)))

(defn- vertical
  [n q]
  (filter (fn [i]
            (and (> i 0)
                 (<= i (Math/pow n 2))))
          (map (fn [i]
                 (+ i (mod q n)))
               (filter
                (fn [i]
                  (if-not (= (type i)
                             clojure.lang.Keyword)
                    (= (mod i n) 0)
                    false))
                (board n)))))

(defn queens-board
  [n q]
  (let [set-queen (assoc (board n) n :q)]))
