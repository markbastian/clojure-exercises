(ns clojure4.unsolved.tree-reparenting-130)

(def t '(a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o)))))

(def goal 'c)

(loop [f (first t) q (rest t)]
  (prn f)
  (if f
    (let [[n & r] (first q)]
      (recur n (into (rest q) r)))
    nil))

(loop [[f l r] t]
  (prn f)
  (when f
    (recur (cons (first l) (cons (rest l) r)))))


