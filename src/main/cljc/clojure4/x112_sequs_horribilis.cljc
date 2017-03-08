(ns clojure4.x112-sequs-horribilis)

;http://www.4clojure.com/problem/112
;Sequs Horribilis
(def __
  (fn sequs
    ([[f & r] coll tot mx]
     (cond
       (nil? f) coll
       (number? f) (let [x (+ tot f)]
                     (if (> x mx) coll (sequs r (conj coll f) x mx)))
       :default (let [t (sequs f [] 0 (- mx tot))
                      x (reduce + (flatten t))]
                  (sequs r (conj coll t) (+ tot x) mx))))
    ([mx s] (sequs s [] 0 mx))))

(assert (=  (__ 10 [1 2 [3 [4 5] 6] 7])
            '(1 2 (3 (4)))))

(assert (=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
            '(1 2 (3 (4 (5 (6 (7))))))))

(assert (=  (__ 9 (range))
            '(0 1 2 3)))

(assert (=  (__ 1 [[[[[1]]]]])
            '(((((1)))))))

(assert (=  (__ 0 [1 2 [3 [4 5] 6] 7])
            '()))

(assert (=  (__ 0 [0 0 [0 [0]]])
            '(0 0 (0 (0)))))

(assert (=  (__ 2 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
            '(-10 (1 (2 3 (4))))))
