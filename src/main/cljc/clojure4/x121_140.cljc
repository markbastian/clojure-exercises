(ns clojure4.x121-140)

;http://www.4clojure.com/problem/121
;Universal Computation Engine
(let [__
      (fn compute [[op & r]]
        (let [ops {'+ + '- - '/ / '* *}]
          (fn [m] (apply
                    (ops op)
                    (map (fn [t]
                           (cond
                             (symbol? t) (m t)
                             (number? t) t
                             :default ((compute t) m))) r)))))]
  (assert (= 2 ((__ '(/ a b))
                 '{b 8 a 16})))
  (assert (= 8 ((__ '(+ a b 2))
                 '{a 2 b 4})))
  (assert (= [6 0 -4]
             (map (__ '(* (+ 2 a)
                          (- 10 b)))
                  '[{a 1 b 8}
                    {b 5 a -2}
                    {a 2 b 11}])))
  (assert (= 1 ((__ '(/ (+ x 2)
                        (* 3 (+ y 1))))
                 '{x 4 y 1}))))

;http://www.4clojure.com/problem/122
;Read a binary number
(let [__ (fn [s] (reduce + (map * (reverse (map {\0 0 \1 1} s)) (iterate #(* 2 %) 1))))]
  (assert (= 0 (__ "0")))
  (assert (= 7 (__ "111")))
  (assert (= 8 (__ "1000")))
  (assert (= 9 (__ "1001")))
  (assert (= 255 (__ "11111111")))
  (assert (= 1365 (__ "10101010101")))
  (assert (= 65535 (__ "1111111111111111"))))

;http://www.4clojure.com/problem/123 - Does not exist

;http://www.4clojure.com/problem/124
;Analyze Reversi
(let [__
      (letfn [(boardwalk [board player-color start-cell dir]
                (let [f (disj #{'b 'w} player-color) g #{player-color}]
                  (when (= 'e (get-in board start-cell))
                    (loop [c (mapv + start-cell dir) res []]
                      (let [x (get-in board c)]
                        (cond
                          (f x) (recur (mapv + c dir) (conj res c))
                          (g x) res
                          :default nil))))))]
        (fn [board color]
          (into {}
                (let [directions [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]]
                  (for [i (range 4) j (range 4) d directions
                        :let [b (boardwalk board color [i j] d)]
                        :when (not-empty b)]
                    [[i j] (set b)])))))]
  (assert (= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
             (__ '[[e e e e]
                   [e w b e]
                   [e b w e]
                   [e e e e]] 'w)))

  (assert (= {[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
             (__ '[[e e e e]
                   [e w b e]
                   [w w w e]
                   [e e e e]] 'b)))

  (assert (= {[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
             (__ '[[e e e e]
                   [e w b e]
                   [w w b e]
                   [e e b e]] 'w)))

  (assert (= {[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}
             (__ '[[e e w e]
                   [b b w e]
                   [b w w e]
                   [b w w w]] 'b))))

;TODO - solve
;http://www.4clojure.com/problem/125
;Gus' Quinundrum
;(= (str '__) (__))

;http://www.4clojure.com/problem/125
;Through the Looking Class
(let [x java.lang.Class]
  (and (= (class x) x) x))

;;http://www.4clojure.com/problem/
;;
;(let [__ false]
;  (assert )
;  (assert )
;  (assert ))