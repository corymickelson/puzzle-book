(ns queens.core)

(defn- board
  [n]
  (vec (range 0 (inc (Math/pow n 2)))))

(defn- rows
  "Bounds defines the ends of the row.
  Ex. for a 4 x 4 board the bounds will be '(0 4 8 12 16)
  Leaving the value of each row a range from indexN to indexN + 1"
  [n]
  (let [bounds (filter (fn [i]
                       (= (mod i n) 0))
                       (board n))]
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

(defn- diagonal-n
  [section rows]
  (map-indexed
   (fn [idx v]
     (let [row (nth rows idx)
           offset-values (list
                          (+ v (inc idx))
                          (- v (inc idx)))]
       (filter (fn [i]
                 (some (partial = i) row))
               offset-values)))
   section))

(defn- diagonal
  [n q]
  (let [rows (rows n)
        base (vertical n q)
        [lower upper] (split-at
                      (first (position (partial = q) base))
                      base)
        [lower-rows upper-rows] (split-at
                                 (first (keep-indexed
                                   (fn [idx i]
                                     (when (some
                                            (partial = q) i)
                                       idx))
                                   rows))
                                 rows)]
    (flatten
     (concat (diagonal-n lower lower-rows)
             (list q)
             (diagonal-n (rest upper) (rest upper-rows))))))


(defn- horizontal
  [n q]
  (let [rows (filter (fn [i]
                       (= (mod i n) 0))
                     (board n))]
    (first (keep-indexed (fn [idx v]
                           (if (> (inc n) (inc idx))
                             (let [row (map inc
                                            (range
                                             (nth rows idx)
                                             (nth rows (inc idx))))]
                               (if (some (partial = q) row)
                                 row))))
                         rows))))

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

(defn queens-path
  [n q]
  (set
   (concat (vertical n q)
           (horizontal n q)
           (diagonal n q))))

(defn queens-board
  [n q]
  (let [queens-verticals (vertical 4 6)
        intersects (set (flatten (horizontal 4 6)
                                (diagonal+ 4 6 queens-verticals)
                                (diagonal- 4 6 queens-verticals)
                                queens-verticals))]
    intersects))
