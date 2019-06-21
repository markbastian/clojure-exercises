(ns clojure-exercises.sequences)

(def fib-seq
  (letfn [(step [[lo hi]] [hi (+ lo hi)])]
    (map first (iterate step [0N 1N]))))

(def pascals-triangle-seq
  (letfn [(step [v] (map (partial apply +) (cons '(1) (partition-all 2 1 v))))]
    (iterate step '(1))))

(def prime-seq
  (letfn [(step [[prime & r]] (remove #(zero? (mod % prime)) r))]
    (map first (iterate step (drop 2 (range))))))

(def prime-seq2
  (->> {:primes [] :n 2}
       (iterate (fn [{:keys [primes n]}]
                  (if (not-any? (fn [prime] (zero? (mod n prime))) primes)
                    {:primes (conj primes n) :n (inc n) :prime n}
                    {:primes primes :n (inc n)})))
       (map :prime)
       (filter identity)))
