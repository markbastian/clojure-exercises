(ns clojure4.x130-tree-reparenting)

(def t '(a
          (b
            (c
              (d) (e))
            (f
              (g) (h)))
          (i
            (j
              (k) (l))
            (m
              (n) (o)))))

(def goal 'c)

(defn reparent [goal t]
  (letfn [(expand [t] (map #(into (list (remove #{%} t)) (reverse %)) (rest t)))
          (expand-seq [s] (mapcat expand s))]
    (->> [t]
         (iterate expand-seq)
         (mapcat (fn [s] (filter (comp #{goal} first) s)))
         first)))

(def __ reparent)

(assert (= '(n) (__ 'n '(n))))

(assert (= '(a (t (e))) (__ 'a '(t (e) (a)))))

(assert
  (= '(e (t (a)))
   (__ 'e '(a (t (e))))))

(assert
  (= '(a (b (c)))
   (__ 'a '(c (b (a))))))

(assert
  (= '(d
        (b
          (c)
          (e)
          (a
            (f
              (g)
              (h)))))
     (__ 'd '(a
               (b
                 (c)
                 (d)
                 (e))
               (f
                 (g)
                 (h))))))

(assert
  (= '(c
        (d)
        (e)
        (b
          (f
            (g)
            (h))
          (a
            (i
              (j
                (k)
                (l))
              (m
                (n)
                (o))))))
     (__ 'c '(a
               (b
                 (c
                   (d)
                   (e))
                 (f
                   (g)
                   (h)))
               (i
                 (j
                   (k)
                   (l))
                 (m
                   (n)
                   (o)))))))