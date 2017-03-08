(ns clojure4.x121-universal-computation-engine)

(def __
  (fn compute [[op & r]]
    (let [ops {'+ + '- - '/ / '* *}]
      (fn [m] (apply
                (ops op)
                (map (fn [t]
                       (cond
                         (symbol? t) (m t)
                         (number? t) t
                         :default ((compute t) m))) r))))))

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
               '{x 4 y 1})))