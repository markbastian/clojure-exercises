(ns clojure4.misc.graph-solver
  (:require [clojure.string :as cs])
  (:import (clojure.lang PersistentQueue)))

(defn expand [{v :visited [[n :as edge]] :queue nbrs :neighbors :as m}]
  (cond-> (update m :queue pop) (not (v n))
          (->
            (update :visited conj edge)
            (update :queue into (map (fn [x] [x n]) (nbrs n))))))

(defn bfs-expander [{:keys[start neighbors]}]
  (iterate expand {:visited {}
                   :queue (conj PersistentQueue/EMPTY [start :done])
                   :neighbors neighbors}))

(defn rsolve-path [goal {:keys [visited]}]
  (when (visited goal) (reverse (take-while (complement #{:done}) (iterate visited goal)))))

(defn bfs-solve [& {:keys [goal] :as ic}]
  (some (fn [{:keys [queue] :as m}]
          (or (rsolve-path goal m) (when (empty? queue) :no-solution)))
        (bfs-expander ic)))

(bfs-solve
  :start 7
  :goal 43
  :neighbors (fn nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (even? x) (conj (/ x 2)))))

(bfs-solve
  :start 1
  :goal 7
  :neighbors {1 [2] 2 [3] 3 [4]})

(def dungeon
  ["XXXXXXXX XXXXXXXX"
   "X     XX XX    TX"
   "X XXX       XXXXX"
   "X XXXX XX XXXXX X"
   "X  XXX XXXX     X"
   "XX  XXXX    XXX X"
   "XXX      XXXXXX X"
   "XXS   XXXXXXXXX X"])

(defn find-cell [d sym]
  (first (for [i (range (count d)) j (range (count (d i)))
               :when (= sym (get-in d [i j]))] [i j])))

(defn n [[i j]]
  (map vector
       ((juxt inc identity dec identity) i)
       ((juxt identity inc identity dec) j)))

(def dungeon-path
  (bfs-solve
    :start (find-cell dungeon \S)
    :goal (find-cell dungeon \T)
    :neighbors (fn [c] (filter #(#{\space \S \T} (get-in dungeon %)) (n c)))))

(if (not= :no-solution dungeon-path)
  (map cs/join
     (reduce #(assoc-in %1 %2 \@)
             (mapv vec dungeon)
             (rest (butlast dungeon-path))))
  "No solution!")

;(take 6 (map :visited (bfs-expander 1 {1 [2] 2 [3] 3 [4]})))
;(time (iterative-number-maze 1 4137))
;(time (iterative-number-maze 7 43))
;(time (iterative-number-maze 9 2))
;(time (iterative-number-maze 2 9))
;(time (iterative-number-maze 2 4))