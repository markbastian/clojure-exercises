(ns clojure4.unsolved.x150-palindromic-numbers)

(defn n->d [n]
  (loop [r (quot n 10) res (list (rem n 10))]
    (if (zero? r)
      (vec res)
      (recur (quot r 10) (conj res (rem r 10))))))

(defn d->n [d] (reduce #(+ (* 10 %1) %2) d))

(defn pal [n]
  (let [d (n->d n)
        c (count d)
        pre-digs (->> d (take (/ (if (even? c) c (inc c)) 2)))
        inc-digs (->> pre-digs d->n inc n->d)
        post-digs (reverse inc-digs)]
    (d->n
      (into
        (if (= (count pre-digs) (count inc-digs))
          inc-digs
          (butlast inc-digs))
        (if (even? c)
          post-digs
          (rest post-digs))))))

;The only thing left is the initial value. Anything not palindromic shouldn't
;be in the seq. Also, 160 -> 161 and 162 -> 171.
(def __
  (letfn [(n->d [n]
            (loop [r (quot n 10) res (list (rem n 10))]
              (if (zero? r)
                (vec res)
                (recur (quot r 10) (conj res (rem r 10))))))
          (d->n [d] (reduce #(+ (* 10 %1) %2) d))
          (pal [n]
            (let [d (n->d n)
                  c (count d)
                  pre-digs (->> d (take (/ (if (even? c) c (inc c)) 2)))
                  inc-digs (->> pre-digs d->n inc n->d)
                  post-digs (reverse inc-digs)]
              (d->n
                (into
                  (if (= (count pre-digs) (count inc-digs))
                    inc-digs
                    (butlast inc-digs))
                  (if (even? c)
                    post-digs
                    (rest post-digs))))))]
    #(iterate pal %)))