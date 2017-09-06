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

(defn branch? [node]
  (prn (str "b: " node))
  (prn (not= node goal))
  (and
    (not= node goal)))

(defn children [node]
  (prn (str "c: " node))
  (identity node))

(tree-seq branch? children t)

(take-while
  #(not= goal %)
  (tree-seq seq? identity t))

(loop [f (first t) q (rest t)]
  (prn f)
  (if f
    (let [[n & r] (first q)]
      (recur n (into (rest q) r)))
    nil))

(let [[rt l r] t]
  r)

;(defn x [[rt l r :as b] o g]
;  (cond
;    (= rt g) (rt l r o)
;    (nil? l) (list rt)
;    :default ))

(defn x [[f & r :as b] p g]
  (cond
    (= f g) (cons b p)
    (empty? r) b
    :default (map #(x % (cons f p) g) r)))

#_(= '(e (t (a)))
   (__ 'e '(a (t (e)))))

(x t () 'c)

(defn x [[n l r] g]
  (if (= n g)
    n
    (cond-> (list n (when r (x r g))) l (conj (x l g)))))




