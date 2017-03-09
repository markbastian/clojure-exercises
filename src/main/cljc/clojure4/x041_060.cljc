(ns clojure4.x041-060)

;http://www.4clojure.com/problem/41
;Drop Every Nth Item
(let [__ #(->> %1
               (partition-all %2)
               (map (partial take (dec %2)))
               flatten)]
  (assert (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (assert (= (__ [:a :b :c :d :e :f] 2) [:a :c :e]))
  (assert (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])))

;http://www.4clojure.com/problem/42
;Factorial Fun
(let [__ #(reduce * (map inc (range %)))]
  (assert (= (__ 1) 1))
  (assert (= (__ 3) 6))
  (assert (= (__ 5) 120)))

;http://www.4clojure.com/problem/43
;Reverse Interleave
(let [__ #(->> %1
               (partition %2)
               (apply map vector))]
  (assert (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
  (assert (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
  (assert (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))))

;http://www.4clojure.com/problem/44
;Rotate Sequence
(let [__ (fn[n s]
           (let[c (count s)
                m (first (filter pos? (iterate #(+ c %) n)))]
             (take c (drop m (cycle s)))))]
  (assert (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2)))
  (assert (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3)))
  (assert (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1)))
  (assert (= (__ 1 '(:a :b :c)) '(:b :c :a)))
  (assert (= (__ -4 '(:a :b :c)) '(:c :a :b))))

;http://www.4clojure.com/problem/45
;Intro to Iterate
(let [__ '(1 4 7 10 13)]
  (assert (= __ (take 5 (iterate #(+ 3 %) 1)))))

;http://www.4clojure.com/problem/46
;Flipping out
(let [__ (fn[f](fn[& args] (apply f (reverse args))))]
  (assert (= 3 ((__ nth) 2 [1 2 3 4 5])))
  (assert (= true ((__ >) 7 8)))
  (assert (= 4 ((__ quot) 2 8)))
  (assert (= [1 2 3] ((__ take) [1 2 3 4 5] 3))))

;http://www.4clojure.com/problem/47
;Contain Yourself
(let [__ 4]
  (assert (contains? #{4 5 6} __))
  (assert (contains? [1 1 1 1 1] __))
  (assert (contains? {4 :a 2 :b} __))
  (assert (not (contains? [1 2 4] __))))

;http://www.4clojure.com/problem/48
;Intro to some
(let [__ 6]
  (assert (= __ (some #{2 7 6} [5 6 7 8])))
  (assert (= __ (some #(when (even? %) %) [5 6 7 8]))))

;http://www.4clojure.com/problem/49
;Split a sequence
(let [__ (fn[n s][(take n s) (drop n s)])]
  (assert (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (assert (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (assert (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

;http://www.4clojure.com/problem/50
;Split by Type
(let [__ #(vals (group-by type %))]
  (assert (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
  (assert (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
  (assert (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})))

;http://www.4clojure.com/problem/51
;Advanced Destructuring
(let [__ [1 2 3 4 5]]
  (assert (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))))

;http://www.4clojure.com/problem/52
;Intro to Destructuring
(assert (= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e])))

;http://www.4clojure.com/problem/53
;Longest Increasing Sub-Seq
(let [__ (fn[sq]
           (loop [[a & r] sq curr [] best []]
             (cond
               (nil? a) (if (and (> (count curr) (count best))
                                 (> (count curr) 1))
                          curr best)
               (or (empty? curr) (> a (peek curr))) (recur r (conj curr a) best)
               :else (recur r [a] (if (and (> (count curr) (count best))
                                           (> (count curr) 1))
                                    curr best)))))]
  (assert (= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3]))
  (assert (= (__ [5 6 1 3 2 7]) [5 6]))
  (assert (= (__ [2 3 3 4 5]) [3 4 5]))
  (assert (= (__ [7 6 5 4]) [])))

;http://www.4clojure.com/problem/54
;Partition a Sequence
(let [__ (fn [n s]
           (loop [[f r] (split-at n s) res []]
             (if (= n (count f))
               (recur (split-at n r) (conj res f))
               res)))]
  (assert (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
  (assert (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
  (assert (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))))

;http://www.4clojure.com/problem/55
;Count Occurrences
(let [__ #(reduce
           (fn [m i]
             (if (m i)
               (update-in m [i] inc)
               (assoc m i 1)))
           {}
           %)]
  (assert (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
  (assert (= (__ [:b :a :b :a :b]) {:a 2, :b 3}))
  (assert (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})))

;http://www.4clojure.com/problem/56
;Find Distinct Items
(let [__ #(loop [[f & r] % fltr #{f} res [f]]
           (if f
             (recur r (conj fltr f) (if (fltr f) res (conj res f)))
             res))]
  (assert (= (__ [1 2 1 3 1 2 4]) [1 2 3 4]))
  (assert (= (__ [:a :a :b :b :c :c]) [:a :b :c]))
  (assert (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
  (assert (= (__ (range 50)) (range 50))))

;http://www.4clojure.com/problem/57
;Simple Recursion
(let [__ '(5 4 3 2 1)]
  (assert (= __ ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))))

;http://www.4clojure.com/problem/58
;Function Composition
(let [__ (fn[& fs]
           (fn[& ic](loop [[f & r] (reverse fs) x (apply f ic)]
                      (if (first r)
                        (recur r ((first r) x))
                        x))))]
  (assert (= [3 2 1] ((__ rest reverse) [1 2 3 4])))
  (assert (= 5 ((__ (partial + 3) second) [1 2 3 4])))
  (assert (= true ((__ zero? #(mod % 8) +) 3 5 7 9)))
  (assert (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))))

;http://www.4clojure.com/problem/59
;Juxtaposition
(let [__ (fn[& fns]
           (fn[& args](map #(apply % args) fns)))]
  (assert (= [21 6 1] ((__ + max min) 2 3 5 1 6 4)))
  (assert (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello")))
  (assert (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))))

;http://www.4clojure.com/problem/60
;Sequence Reductions
(let [__ (fn red
           ([f v [s & r]] (lazy-seq (cons v (when s (red f (f v s) r)))))
           ([f [s & r]] (red f s r)))]
  (assert (= (take 5 (__ + (range))) [0 1 3 6 10]))
  (assert (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
  (assert (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)))