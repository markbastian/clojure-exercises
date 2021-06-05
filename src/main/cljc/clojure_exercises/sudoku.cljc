(ns clojure-exercises.sudoku
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [clojure-exercises.sudoku-input :as ex]))

(def valid-values (apply hash-set (range 1 10)))

(defonce all-cells (for [i (range 9) j (range 9)] [i j]))

(defonce neighbors
         (let [cells
               (juxt
                 #(map (fn [i] [i (second %)]) (range 9))
                 #(map (fn [j] [(first %) j]) (range 9))
                 #(for [a (range 3) b (range 3)]
                    (mapv + [(* 3 (quot (first %) 3)) (* 3 (quot (second %) 3))] [a b])))]
           (zipmap all-cells (map #(disj (reduce into #{} (cells %)) %) all-cells))))

(defn lock-cell [[board unknowns] cell v]
  [(assoc-in board cell v)
   (reduce
     (fn [u n] (cond-> u (u n) (update n disj v)))
     (dissoc unknowns cell)
     (neighbors cell))])

(defn initialize [initial-board]
  (reduce
    (fn [[board :as soln] cell]
      (if-let [value (valid-values (get-in board cell))]
        (lock-cell soln cell value)
        soln))
    [initial-board (zipmap all-cells (repeat valid-values))]
    all-cells))

(comment
  (initialize ex/easy))

(defn solve-step [[_ unknowns :as soln]]
  ;Get the most constrained cell
  (let [[best-cell best-values] (apply min-key (comp count val) unknowns)]
    ;produce a vector of solutions with that cell locked
    (for [v best-values] (lock-cell soln best-cell v))))

(defn solve [board]
  (loop [[[board unknowns :as f] & r] [(initialize board)]]
    (cond
      (and (empty? unknowns) (every? #(every? integer? %) board)) board
      (nil? f) f
      :default (recur (into r (solve-step f))))))

(comment
  (solve-step (initialize ex/brutal)))

(defn valid-cell? [board cell]
  (not-any? #{(get-in board cell)}
            (map (partial get-in board)
                 (neighbors cell))))

(defn bad-cells [board]
  (remove (partial valid-cell? board) all-cells))

(defn valid-board? [board]
  (and
    (every? (partial get-in board) all-cells)
    (empty? (bad-cells board))))

(comment
  (do
    (tufte/add-basic-println-handler! {})
    (profile
      {:dynamic? true}
      (dotimes [_ 5]
        (p :mbastian (doall (map solve ex/all-example-problems)))
        (p :easy (solve ex/easy))
        (p :broken (solve ex/broken))
        (p :hard (solve ex/hard))
        (p :absurd (solve ex/absurd))
        (p :brutal (solve ex/brutal))
        (p :hardest (solve ex/hardest))
        (p :al-escargot (solve ex/al-escargot))
        (p :hard-17 (solve ex/hard-17))
        (p :init (initialize ex/hard-17))))))
