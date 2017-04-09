(ns queens.queen
  (:require [queens.board :refer [blank-board rows position]]))

(defn- horizontal
  [n q]
  (let [rows (filter (fn [i]
                       (= (mod i n) 0))
                     (blank-board n))]
    (first
     (keep-indexed (fn [idx v]
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
                (blank-board n)))))

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
        [lower-rows upper-rows]
        (split-at
         (first (keep-indexed
                 (fn [idx i]
                   (when (some
                          (partial = q) i)
                     idx))
                 rows))
         rows)]
    (flatten
     (concat (diagonal-n (reverse lower) (reverse lower-rows))
             (list q)
             (diagonal-n (rest upper) (rest upper-rows))))))

(defn queens-path
  [n q]
  (set
   (concat (vertical n q)
           (horizontal n q)
           (diagonal n q))))
