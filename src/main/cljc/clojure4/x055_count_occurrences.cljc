(ns clojure4.x055-count-occurrences)

;http://www.4clojure.com/problem/55
;Count Occurrences

(def __
  #(reduce
     (fn [m i]
       (if (m i)
         (update-in m [i] inc)
         (assoc m i 1)))
     {}
     %))

(assert (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
(assert (= (__ [:b :a :b :a :b]) {:a 2, :b 3}))
(assert (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))