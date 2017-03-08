(ns clojure4.x019-x-easy)

;http://www.4clojure.com/problem/19
;Last Element
(let [__ #(first (reverse %))]
  (assert (= (__ [1 2 3 4 5]) 5))
  (assert (= (__ '(5 4 3)) 3))
  (assert (= (__ ["b" "c" "d"]) "d")))

;Alternate solution using composition
(let [__ (comp first reverse)]
  (assert (= (__ [1 2 3 4 5]) 5))
  (assert (= (__ '(5 4 3)) 3))
  (assert (= (__ ["b" "c" "d"]) "d")))

;http://www.4clojure.com/problem/20
;Penultimate Element
(let [__ #(-> % butlast last)]
  (assert (= (__ (list 1 2 3 4 5)) 4))
  (assert (= (__ ["a" "b" "c"]) "b"))
  (assert (= (__ [[1 2] [3 4]]) [1 2])))

(let [__ (comp last butlast)]
  (assert (= (__ (list 1 2 3 4 5)) 4))
  (assert (= (__ ["a" "b" "c"]) "b"))
  (assert (= (__ [[1 2] [3 4]]) [1 2])))

;http://www.4clojure.com/problem/20
;Nth Element
(let [__ #(first (drop %2 %1))]
  (assert (= (__ '(4 5 6 7) 2) 6))
  (assert (= (__ [:a :b :c] 0) :a))
  (assert (= (__ [1 2 3 4] 1) 2))
  (assert (= (__ '([1 2] [3 4] [5 6]) 2) [5 6])))