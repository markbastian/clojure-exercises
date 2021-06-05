(ns clojure-exercises.fibs)

(defn nth-fib [n]
  (loop [i 0 j 1 step 0]
    (if (= step n)
      i
      (recur j (+ i j) (inc step)))))

(defn n-fibs [n]
  (loop [i 0 j 1 step 0 res []]
    (if (= step n)
      res
      (recur j (+ i j) (inc step) (conj res i)))))

;Compute
(defn fib-step [[i j]] [j (+ i j)])

;Iterate
(def fib-seq (map first (iterate fib-step [0N 1N])))

;Terminate
(def nth-fib (partial nth fib-seq))

;Terminate
(defn n-fibs [n] (take n fib-seq))

(comment
  (def phi
    (->> fib-seq
         (partition 2 1)
         rest
         (map (fn [[den num]] (double (/ num den))))
         (partition 2 1)
         (some (fn [[a b]] (when (= a b) a))))))

