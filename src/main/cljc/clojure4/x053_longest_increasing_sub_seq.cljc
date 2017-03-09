(ns clojure4.x053-longest-increasing-sub-seq)

;http://www.4clojure.com/problem/53
;Longest Increasing Sub-Seq

(def __
  (fn[sq]
    (loop [[a & r] sq curr [] best []]
      (cond
        (nil? a) (if (and (> (count curr) (count best))
                          (> (count curr) 1))
                   curr best)
        (or (empty? curr) (> a (peek curr))) (recur r (conj curr a) best)
        :else (recur r [a] (if (and (> (count curr) (count best))
                                    (> (count curr) 1))
                             curr best))))))

(assert (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3]))
(assert (= (__ [5 6 1 3 2 7]) [5 6]))
(assert (= (__ [2 3 3 4 5]) [3 4 5]))
(assert (= (__ [7 6 5 4]) []))
