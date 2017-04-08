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

;http://www.4clojure.com/problem/125
;Gus' Quinundrum
(assert
  (=
    (str '(fn [] (let [q (str (quote (fn [] (let [q (str (quote %s))] (format q q)))))] (format q q))))
    ((fn [] (let [q (str (quote (fn [] (let [q (str (quote %s))] (format q q)))))] (format q q))))))

;http://www.4clojure.com/problem/126
;Through the Looking Class
(let [x java.lang.Class]
  (and (= (class x) x) x))

;http://www.4clojure.com/problem/127
;Love Triangle
(let [__
      (fn [pattern]
        (letfn [(n-bits [n m] (mapv #(if (bit-test m %) 1 0) (range n)))
                (decode [s] (mapv (partial n-bits (int (Math/ceil (/ (Math/log (apply max s)) (Math/log 2))))) s))
                (grow-up [[i j]] [[(inc i) j] [(inc i) (dec j)]])
                (grow-dn [[i j]] [[(inc i) j] [(inc i) (inc j)]])
                (grow-a [[i j]] [[(inc i) (inc j)][(inc i) j][(inc i) (dec j)]])
                (grow-b [[i j]] [[(dec i) (inc j)][(dec i) j][(dec i) (dec j)]])
                (grow-c [[i j]] [[(inc i) (inc j)][i (inc j)][(dec i) (inc j)]])
                (grow-d [[i j]] [[(inc i) (dec j)][i (dec j)][(dec i) (dec j)]])
                (score [rock start f]
                  (->> #{start}
                       (iterate (fn[s] (into s (mapcat f s))))
                       (take-while (fn [r] (every? #(#{1} (get-in rock %)) r)))
                       rest
                       last
                       count))]
          (let [rock (decode pattern)
                scores
                (not-empty
                  (for [i (range (count rock)) j (range (count (rock i))) f [grow-up grow-dn grow-a grow-b grow-c grow-d]
                        :let [score (score rock [i j] f)] :when (> score 2)] score))]
            (when scores (apply max scores)))))]
  (assert (= 10 (__ [15 15 15 15 15])))
  (assert (= 15 (__ [1 3 7 15 31])))
  (assert (= 3 (__ [3 3])))
  (assert (= 4 (__ [7 3])))
  (assert (= 6 (__ [17 22 6 14 22])))
  (assert (= 9 (__ [18 7 14 14 6 3])))
  (assert (= nil (__ [21 10 21 10])))
  (assert (= nil (__ [0 31 0 31 0]))))

;http://www.4clojure.com/problem/131
;Sum Some Set Subsets
(let [__
(fn [& items]
  (letfn [(expand [items sets]
            (into sets
                  (for [st sets item items :when ((complement st) item)]
                    (conj st item))))
          (combos [items]
            (let [maxiter (count items)
                  start (into #{} (map hash-set items))]
              (into #{} (map #(reduce + %) (nth (iterate (partial expand items) start) maxiter)))))]
    (some? (not-empty (apply clojure.set/intersection (map combos items))))))]
  (= true  (__ #{-1 1 99}
               #{-2 2 888}
               #{-3 3 7777}))
  (assert (= false (__ #{1}
                       #{2}
                       #{3}
                       #{4})))
  (assert (= true  (__ #{1})))
  (assert (= false (__ #{1 -3 51 9}
                       #{0}
                       #{9 2 81 33})))
  (assert (= true  (__ #{1 3 5}
                       #{9 11 4}
                       #{-3 12 3}
                       #{-3 4 -2 10})))
  (assert (= false (__ #{-1 -2 -3 -4 -5 -6}
                       #{1 2 3 4 5 6 7 8 9})))
  (assert (= true  (__ #{1 3 5 7}
                       #{2 4 6 8})))
  (assert (= true  (__ #{-1 3 -5 7 -9 11 -13 15}
                       #{1 -3 5 -7 9 -11 13 -15}
                       #{1 -1 2 -2 4 -4 8 -8})))
  (assert (= true  (__ #{-10 9 -8 7 -6 5 -4 3 -2 1}
                       #{10 -9 8 -7 6 -5 4 -3 2 -1}))))

;;http://www.4clojure.com/problem/
;;
;(let [__ false]
;  (assert )
;  (assert )
;  (assert ))