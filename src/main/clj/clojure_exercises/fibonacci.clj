(ns clojure-exercises.fibonacci)

(def fib-seq (map first (iterate (fn [[i j]][j (+ i j)]) [0N 1N])))

(defn fib [n]
  (case n
    (0 1) n
    (+ (fib (- n 1)) (fib (- n 2)))))

(def memo-fib (memoize fib))