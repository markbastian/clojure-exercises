(ns clojure4.x081-100)

;http://www.4clojure.com/problem/81
;Set Intersection
(let [__ #(into #{} (filter %1 %2))]
  (assert (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3}))
  (assert (= (__ #{0 1 2} #{3 4 5}) #{}))
  (assert (= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})))

;http://www.4clojure.com/problem/82
;Word Chains
(let [__ (fn word-path [opts]
           (letfn [(one-off? [m n]
                     (loop[[a & r :as x] m [b & s :as y] n d 0]
                       (let [cx (count x) cy (count y)]
                         (cond
                           (> d 1) false
                           (and (nil? a) (nil? b)) (<= d 1)
                           (= a b) (recur r s d)
                           (= cx cy) (recur r s (inc d))
                           (> cx cy) (recur r y (inc d))
                           (< cx cy) (recur x s (inc d))))))
                   (expand [paths]
                     (for [path paths n (filter #(one-off? (peek path) %) opts)
                           :when (not-any? #{n} path)] (conj path n)))]
             (not (empty? (nth (iterate expand (map vector opts)) (dec (count opts)))))))]
  (assert (= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
  (assert (= false (__ #{"cot" "hot" "bat" "fat"})))
  (assert (= false (__ #{"to" "top" "stop" "tops" "toss"})))
  (assert (= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"})))
  (assert (= true (__ #{"share" "hares" "shares" "hare" "are"})))
  (assert (= false (__ #{"share" "hares" "hare" "are"}))))

;http://www.4clojure.com/problem/83
;A Half-Truth
(let [__ (fn[& r](= 2 (count (distinct r))))]
  (assert (= false (__ false false)))
  (assert (= true (__ true false)))
  (assert (= false (__ true)))
  (assert (= true (__ false true false)))
  (assert (= false (__ true true true)))
  (assert (= true (__ true true true false))))

;TODO
;;http://www.4clojure.com/problem/85
;Transitive Closure
#_(let [__ false]
  (assert (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
            (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))
  (assert (let [more-legs
                #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
            (= (__ more-legs)
               #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
                 ["spider" "cat"] ["spider" "man"] ["spider" "snake"]})))
  (assert (let [progeny
                #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
            (= (__ progeny)
               #{["father" "son"] ["father" "grandson"]
                 ["uncle" "cousin"] ["son" "grandson"]}))))

;http://www.4clojure.com/problem/85
;Power Set
(let [__ (fn[s]
           (loop[[f & r] (into [] s) res #{#{}}]
             (if f
               (recur r (into res (map #(conj % f) res)))
               res)))]
  (assert (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
  (assert (= (__ #{}) #{#{}}))
  (assert (= (__ #{1 2 3})
             #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
  (assert (= (count (__ (into #{} (range 10)))) 1024)))

;http://www.4clojure.com/problem/86
;Happy numbers
(let [__ (fn[start]
           (let [f (fn [n]
                     (->>
                       (loop [r (quot n 10) s [(mod n 10)]]
                         (if (zero? r) s (recur (quot r 10) (conj s (mod r 10)))))
                       (map (fn [x] (* x x)))
                       (reduce +)))]
             (loop [fx (f start) fails #{}]
               (cond
                 (#{1} fx) true
                 (fails fx) false
                 :else (recur (f fx) (conj fails fx))))))]
  (assert (= (__ 7) true))
  (assert (= (__ 986543210) true))
  (assert (= (__ 2) false))
  (assert (= (__ 3) false)))

;http://www.4clojure.com/problem/87 - DOES NOT EXIST!

;http://www.4clojure.com/problem/88
;Symmetric Difference
(let [__ (fn[a b]
           (-> #{}
               (into (remove b a))
               (into (remove a b))))]
  (assert (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
  (assert (= (__ #{:a :b :c} #{}) #{:a :b :c}))
  (assert (= (__ #{} #{4 5 6}) #{4 5 6}))
  (assert (= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

;TODO
;http://www.4clojure.com/problem/89
;Graph Tour
#_(let [__ false]
    (assert (= true (__ [[:a :b]])))
    (assert (= false (__ [[:a :a] [:b :b]])))
    (assert (= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
                          [:a :d] [:b :d] [:c :d]])))
    (assert (= true (__ [[1 2] [2 3] [3 4] [4 1]])))
    (assert (= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
                         [:b :e] [:a :d] [:b :d] [:c :e]
                         [:d :e] [:c :f] [:d :f]])))
    (assert (= false (__ [[1 2] [2 3] [2 4] [2 5]]))))

;http://www.4clojure.com/problem/90
;Cartesian Product
(let [__ (fn[a b](into #{}(for[ai a bi b][ai bi])))]
  (assert (= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
             #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
               ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
               ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
  (assert (= (__ #{1 2 3} #{4 5})
             #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
  (assert (= 300 (count (__ (into #{} (range 10))
                            (into #{} (range 30)))))))

;;http://www.4clojure.com/problem/
;;
;(let [__ false]
;  (assert )
;  (assert )
;  (assert ))
