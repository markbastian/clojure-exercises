(ns clojure4.x150-palindromic-numbers)

(defn n->d [n]
  (loop [r (quot n 10) res (list (rem n 10))]
    (if (zero? r)
      (vec res)
      (recur (quot r 10) (conj res (rem r 10))))))

(defn d->n [d] (reduce #(+ (* 10 %1) %2) d))

(defn pal-up [c op half-digits]
  (let [inc-digs (->> half-digits d->n op n->d)
        post-digs (reverse inc-digs)]
    (d->n
      (into
        (if (= (count half-digits) (count inc-digs))
          inc-digs
          (vec (butlast inc-digs)))
        (if (even? c)
          post-digs
          (rest post-digs))))))

(defn next-palindromic-number [n]
  (let [d (n->d n)
        c (count d)
        i (/ (if (even? c) c (inc c)) 2)
        ;Get the first and last half of the digits, inclusive of the central digit in both cases.
        pre-digs (take i d)
        rem-digs (take i (reverse d))]
    (println {:n n
              :pre pre-digs
              :rem rem-digs
              :x (< (d->n pre-digs) (d->n rem-digs))})
    ;If the number is a palindrome or the trailing digits are greater than the
    ;leading digits, we need increase the center digit.
    (if (or (= pre-digs rem-digs)
            (< (d->n pre-digs) (d->n rem-digs)))
      (pal-up c inc pre-digs)
      ;Otherwise reverse the truncated digits
      (pal-up c identity pre-digs))))

(comment
  (next-palindromic-number 141)
  (next-palindromic-number 147)
  (next-palindromic-number 741)
  (next-palindromic-number 21)
  (next-palindromic-number 12)
  )

(defn palindromic-number? [n]
  (let [digs (n->d n)]
    (= digs (reverse digs))))

(comment
  (palindromic-number? 123)
  (palindromic-number? 121)
  )

(defn palindrome-seq [from]
  (let [start (if (palindromic-number? from) from (next-palindromic-number from))]
    (iterate next-palindromic-number start)))

(comment
  (take 10 (palindrome-seq 100)))

;The only thing left is the initial value. Anything not palindromic shouldn't
;be in the seq. Also, 160 -> 161 and 162 -> 171.
(def __
  (letfn [(n->d [n]
            (loop [r (quot n 10) res (list (rem n 10))]
              (if (zero? r)
                (vec res)
                (recur (quot r 10) (conj res (rem r 10))))))
          (d->n [d] (reduce #(+ (* 10 %1) %2) d))
          (pal-up [c op digs]
            (let [inc-digs (->> digs d->n op n->d)
                  post-digs (reverse inc-digs)]
              (d->n
                (into
                  (if (= (count digs) (count inc-digs))
                    inc-digs
                    (vec (butlast inc-digs)))
                  (if (even? c)
                    post-digs
                    (rest post-digs))))))
          (pal? [n] (let [digs (n->d n)] (= digs (reverse digs))))
          (pal [n]
            (let [d (n->d n)
                  c (count d)
                  i (/ (if (even? c) c (inc c)) 2)
                  pre-digs (take i d)
                  rem-digs (take i (reverse d))]
              (if (or (= pre-digs rem-digs)
                      (< (d->n pre-digs) (d->n rem-digs)))
                (pal-up c inc pre-digs)
                (pal-up c identity pre-digs))))]
    #(iterate pal (if (pal? %) % (pal %)))))

(assert
  (= (take 26 (__ 0))
     [0 1 2 3 4 5 6 7 8 9
      11 22 33 44 55 66 77 88 99
      101 111 121 131 141 151 161]))

(assert
  (= (take 16 (__ 162))
     [171 181 191 202
      212 222 232 242
      252 262 272 282
      292 303 313 323]))

(assert
  (= (take 6 (__ 1234550000))
     [1234554321 1234664321 1234774321
      1234884321 1234994321 1235005321]))

(assert
  (= (first (__ (* 111111111 111111111)))
     (* 111111111 111111111)))

(assert
  (= (set (take 199 (__ 0)))
     (set (map #(first (__ %)) (range 0 10000)))))

(assert
  (= true
     (apply < (take 6666 (__ 9999999)))))

(assert
  (= (nth (__ 0) 10101)
     9102019))