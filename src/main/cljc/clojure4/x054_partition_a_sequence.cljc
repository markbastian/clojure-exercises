(ns clojure4.x054-partition-a-sequence)

;http://www.4clojure.com/problem/54
;Partition a Sequence

(def __
  (fn [n s]
    (loop [[f r] (split-at n s) res []]
      (if (= n (count f))
        (recur (split-at n r) (conj res f))
        res))))

(assert (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
(assert (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
(assert (= (__ 3 (range 8)) '((0 1 2) (3 4 5))))