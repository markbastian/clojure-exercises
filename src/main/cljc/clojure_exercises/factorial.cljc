(ns clojure-exercises.factorial)

(defn factorial [n]
  (reduce * (map (comp bigint inc) (range n))))
