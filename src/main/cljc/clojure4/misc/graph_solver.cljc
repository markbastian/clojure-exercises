(ns clojure4.misc.graph-solver
  (:import (clojure.lang PersistentQueue)))

(defn expand [{v :visited [[n :as edge]] :queue nbrs :neighbors :as m}]
  (cond-> (update m :queue pop) (not (v n))
          (->
            (update :visited conj edge)
            (update :queue into (map (fn [x] [x n]) (nbrs n))))))

(defn bfs-expander [start fn-neighbors]
  (iterate expand {:visited {}
                   :queue (conj PersistentQueue/EMPTY [start :done])
                   :neighbors fn-neighbors}))

(defn rsolve-path [goal {:keys [visited]}]
  (when (visited goal) (reverse (take-while (complement #{:done}) (iterate visited goal)))))

(defn solve-path [s g f]
  (some (fn [{:keys [queue] :as m}]
          (or (rsolve-path g m) (when (empty? queue) :no-solution))) (bfs-expander s f)))

(solve-path 7 43 (fn nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (zero? (mod x 2)) (conj (/ x 2)))))

(solve-path 1 7 {1 [2] 2 [3] 3 [4]})

(take 6 (map :visited (bfs-expander 1 {1 [2] 2 [3] 3 [4]})))

(map first (take-while #(apply not= (map :visited %))
      (partition 2 1 (iterate expand {:visited {}
                                      :queue (conj PersistentQueue/EMPTY [1 :done])
                                      :neighbors {1 [2] 2 [3] 3 [4]}}))))