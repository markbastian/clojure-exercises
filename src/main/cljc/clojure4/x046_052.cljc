(ns clojure4.x046-052)

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