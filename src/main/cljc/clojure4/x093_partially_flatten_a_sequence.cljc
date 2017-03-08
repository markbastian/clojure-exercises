(ns clojure4.x093-partially-flatten-a-sequence)

;http://www.4clojure.com/problem/93
;Partially Flatten a Sequence

(def __
  (fn parflat [s]
    (if (not-any? coll? s) [s] (mapcat parflat s))))

(assert (= (__ [["Do"] ["Nothing"]])
           [["Do"] ["Nothing"]]))

(assert (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
           [[:a :b] [:c :d] [:e :f]]))

(assert (= (__ '((1 2)((3 4)((((5 6)))))))
           '((1 2)(3 4)(5 6))))