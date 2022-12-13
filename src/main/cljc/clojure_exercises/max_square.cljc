(ns clojure-exercises.max-square
  (:require [clojure.pprint :as pp]))

(def m [[0 1 1 0 1]
        [1 1 0 1 0]
        [0 1 1 1 0]
        [1 1 1 1 0]
        [1 1 1 1 1]
        [0 0 0 0 0]])

(defn max-square [matrix]
  (reduce
    (fn [{:keys [m max-square] :as acc} [i j :as coord]]
      (if (zero? (get-in m coord))
        (assoc-in acc [:m i j] 0)
        (let [v (inc (min
                       (get-in m [(dec i) j])
                       (get-in m [i (dec j)])
                       (get-in m [(dec i) (dec j)])))]
          {:m          (assoc-in m coord v)
           :max-square (max max-square v)})))
    {:m matrix :max-square 0}
    (for [i (range 1 (count matrix))
          j (range 1 (count (first matrix)))]
      [i j])))

(comment
  (let [matrix m]
    (for [i (range 1 (count matrix))
          j (range 1 (count (first matrix)))]
      [i j]))

  (:max-square (max-square m)))
