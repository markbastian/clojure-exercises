(ns clojure-exercises.reverse-int)

(defn reverse-int [n]
  (loop [a (quot n 10) res (rem n 10)]
    (if (pos? a)
      (recur (quot a 10) (+ (* 10 res) (rem a 10)))
      res)))

(defn digits [n]
  (loop [a (quot n 10) res (list (rem n 10))]
    (if (pos? a)
      (recur (quot a 10) (conj res (rem a 10)))
      (vec res))))

(defn rint [n]
  (reduce #(+ (* 10 %1) %2) (rseq (digits n))))