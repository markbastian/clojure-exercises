(ns clojure4.unsolved.x089-graph-tour)

;(def __)
;
;
;(assert (= true (__ [[:a :b]])))
;(assert (= false (__ [[:a :a] [:b :b]])))
;(assert (= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
;                      [:a :d] [:b :d] [:c :d]])))
;(assert (= true (__ [[1 2] [2 3] [3 4] [4 1]])))
;(assert (= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
;                     [:b :e] [:a :d] [:b :d] [:c :e]
;                     [:d :e] [:c :f] [:d :f]])))
;(assert (= false (__ [[1 2] [2 3] [2 4] [2 5]])))

(def original-edges [[:a :b] [:a :c] [:c :b] [:a :e]
                     [:b :e] [:a :d] [:b :d] [:c :e]
                     [:d :e] [:c :f] [:d :f]])
(def edges (into #{} original-edges))
(def starts (map vector edges))

(defn expand [paths]
  (for [path paths
        unvisited (filter #(= (second (peek path)) (first %)) (reduce (fn [s p] (disj s p)) edges path))]
    (conj path unvisited)))

(take 2 (iterate expand starts))