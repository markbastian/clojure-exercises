(ns clojure4.unsolved.tree-reparenting-130)

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

(defn doit [goal [p l r] v]
  (cond
    (nil? p) nil
    (= p goal) [p l r v]
    :default (or (doit goal l (into v [p r]))
                 (doit goal r (into v [p l])))))

(doit 'b t [])
