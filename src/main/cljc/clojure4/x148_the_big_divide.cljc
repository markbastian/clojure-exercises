(ns clojure4.x148-the-big-divide)

(defn tri [n] (/ (* n (inc n)) 2))

(defn bd [n a b]
  (letfn [(f [x](if (zero? (rem n x)) (dec n) n))]
    (let [c (* b a)]
      (+ (* a (tri (quot (f a) a)))
         (* b (tri (quot (f b) b)))
         (* -1 c (tri (quot (f c) c)))))))

(def __
  (fn [n a b]
    (letfn [(f [x](if (zero? (rem n x)) (dec n) n))
            (tri [n] (/ (* n (inc n)) 2))]
      (let [ab (bigint a) bb (bigint b) cb (* bb ab)]
        (+ (* ab (tri (quot (f ab) ab)))
           (* bb (tri (quot (f bb) bb)))
           (* -1 cb (tri (quot (f cb) cb))))))))

(assert (= 0 (__ 3 17 11)))
(assert (= 23 (__ 10 3 5)))
(assert (= 233168 (__ 1000 3 5)))
(assert (= "2333333316666668" (str (__ 100000000 3 5))))
(assert (= "110389610389889610389610" (str (__ (* 10000 10000 10000) 7 11))))
(assert (= "1277732511922987429116" (str (__ (* 10000 10000 10000) 757 809))))
(assert (= "4530161696788274281" (str (__ (* 10000 10000 1000) 1597 3571))))
