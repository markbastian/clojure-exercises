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

;http://www.4clojure.com/problem/21
;Nth Element
(let [__ #(first (drop %2 %1))]
  (assert (= (__ '(4 5 6 7) 2) 6))
  (assert (= (__ [:a :b :c] 0) :a))
  (assert (= (__ [1 2 3 4] 1) 2))
  (assert (= (__ '([1 2] [3 4] [5 6]) 2) [5 6])))

;http://www.4clojure.com/problem/22
;Count a Sequence
(let [__ #(reduce (fn[s n](inc s)) 0 %)]
  (assert (= (__ '(1 2 3 3 1)) 5))
  (assert (= (__ "Hello World") 11))
  (assert (= (__ [[1 2] [3 4] [5 6]]) 3))
  (assert (= (__ '(13)) 1))
  (assert (= (__ '(:a :b :c)) 3)))

;http://www.4clojure.com/problem/23
;Reverse a Sequence
(let [__ #(loop[s % res ()]
            (if (first s)
              (recur (rest s) (conj res (first s)))
              res))]
  (assert (= (__ [1 2 3 4 5]) [5 4 3 2 1]))
  (assert (= (__ (sorted-set 5 7 2 7)) '(7 5 2)))
  (assert (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

;Using destructuring - 4Clojure seems to barf on this
(let [__ #(loop[[f & r] % res ()]
            (if f (recur r (conj res f)) res))]
  (assert (= (__ [1 2 3 4 5]) [5 4 3 2 1]))
  (assert (= (__ (sorted-set 5 7 2 7)) '(7 5 2)))
  (assert (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

;http://www.4clojure.com/problem/24
;Sum It All Up
(let [__ #(reduce + %)]
  (assert (= (__ [1 2 3]) 6))
  (assert (= (__ (list 0 -2 5 5)) 8))
  (assert (= (__ #{4 2 1}) 7))
  (assert (= (__ '(0 0 -1)) -1))
  (assert (= (__ '(1 10 3)) 14)))
