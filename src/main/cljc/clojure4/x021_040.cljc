(ns clojure4.x021-040)

(let [__ (comp last butlast)]
  (assert (= (__ (list 1 2 3 4 5)) 4))
  (assert (= (__ ["a" "b" "c"]) "b"))
  (assert (= (__ [[1 2] [3 4]]) [1 2])))

;http://www.4clojure.com/problem/21
;Nth Element
(let [__ #(first (drop %2 %1))]
  (assert (= (__ '(4 5 6 7) 2) 6))
  (assert (= (__ [:a :b :c] 0) :a))
  (assert (= (__ [1 2 3 4] 1) 2))
  (assert (= (__ '([1 2] [3 4] [5 6]) 2) [5 6])))

;http://www.4clojure.com/problem/22
;Count a Sequence
(let [__ #(reduce (fn[s n](inc s)) 0 %)]
  (assert (= (__ '(1 2 3 3 1)) 5))
  (assert (= (__ "Hello World") 11))
  (assert (= (__ [[1 2] [3 4] [5 6]]) 3))
  (assert (= (__ '(13)) 1))
  (assert (= (__ '(:a :b :c)) 3)))

;http://www.4clojure.com/problem/23
;Reverse a Sequence
(let [__ #(loop[s % res ()]
            (if (first s)
              (recur (rest s) (conj res (first s)))
              res))]
  (assert (= (__ [1 2 3 4 5]) [5 4 3 2 1]))
  (assert (= (__ (sorted-set 5 7 2 7)) '(7 5 2)))
  (assert (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

;Using destructuring - 4Clojure seems to barf on this
(let [__ #(loop[[f & r] % res ()]
            (if f (recur r (conj res f)) res))]
  (assert (= (__ [1 2 3 4 5]) [5 4 3 2 1]))
  (assert (= (__ (sorted-set 5 7 2 7)) '(7 5 2)))
  (assert (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

;http://www.4clojure.com/problem/24
;Sum It All Up
(let [__ #(reduce + %)]
  (assert (= (__ [1 2 3]) 6))
  (assert (= (__ (list 0 -2 5 5)) 8))
  (assert (= (__ #{4 2 1}) 7))
  (assert (= (__ '(0 0 -1)) -1))
  (assert (= (__ '(1 10 3)) 14)))

;http://www.4clojure.com/problem/25
;Find the odd numbers
(let [__ #(filter odd? %)]
  (assert (= (__ #{1 2 3 4 5}) '(1 3 5)))
  (assert (= (__ [4 2 1 6]) '(1)))
  (assert (= (__ [2 2 4 6]) '()))
  (assert (= (__ [1 1 1 3]) '(1 1 1 3))))

;http://www.4clojure.com/problem/26
;Fibonacci Sequence
(let [__ #(map first (take % (iterate (fn[[f r]][r (+ f r)]) [1 1])))]
  (assert (= (__ 3) '(1 1 2)))
  (assert (= (__ 6) '(1 1 2 3 5 8)))
  (assert (= (__ 8) '(1 1 2 3 5 8 13 21))))

;Using lazy seqs
(letfn [(fib
          ([a b] (lazy-seq (cons a (fib b (+ a b)))))
          ([] (fib 1 1)))]
  (let [__ #(take % (fib))]
    (assert (= (__ 3) '(1 1 2)))
    (assert (= (__ 6) '(1 1 2 3 5 8)))
    (assert (= (__ 8) '(1 1 2 3 5 8 13 21)))))

;http://www.4clojure.com/problem/27
;Palindrome Detector
(let [__ #(= (seq %) (reverse %))]
  (assert (false? (__ '(1 2 3 4 5))))
  (assert (true? (__ "racecar")))
  (assert (true? (__ [:foo :bar :foo])))
  (assert (true? (__ '(1 1 3 3 1 1))))
  (assert (false? (__ '(:a :b :c)))))

;http://www.4clojure.com/problem/28
;Flatten a Sequence
(let [__ (fn[l](first
                 (first (drop-while
                          #(not= (first %) (second %))
                          (partition 2 (iterate #(reduce
                                                   (fn [c i]
                                                     (cond
                                                       (coll? i) (into c i)
                                                       :else (conj c i))) [] %) l))))))]
  (assert (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (assert (= (__ ["a" ["b"] "c"]) '("a" "b" "c")))
  (assert (= (__ '((((:a))))) '(:a))))

;http://www.4clojure.com/problem/29
;Get the Caps
(let [__ #(apply str (filter (fn[c](<= (int \A) (int c) (int \Z))) %))]
  (assert (= (__ "HeLlO, WoRlD!") "HLOWRD"))
  (assert (empty? (__ "nothing")))
  (assert (= (__ "$#A(*&987Zf") "AZ")))

;http://www.4clojure.com/problem/30
;Compress a Sequence
(let [__ #(reduce
            (fn [c f] (if (= (peek c) f) c (conj c f)))
            [] %)]
  (assert (= (apply str (__ "Leeeeeerrroyyy")) "Leroy"))
  (assert (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (assert (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

;http://www.4clojure.com/problem/31
;Pack a Sequence
(let [__ #(loop [[f r] (split-with #{(first %)} %) res []]
            (if (first r)
              (recur (split-with #{(first r)} r) (conj res f))
              (conj res f)))]
  (assert (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
  (assert (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
  (assert (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))))

;http://www.4clojure.com/problem/32
;Duplicate a Sequence
(let [__ #(interleave % %)]
  (assert (= (__ [1 2 3]) '(1 1 2 2 3 3)))
  (assert (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
  (assert (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
  (assert (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

;http://www.4clojure.com/problem/33
;Replicate a Sequence
(let [__ #(if (= 1 %2) %1 (apply interleave (repeat %2 %1)))]
  (assert (= (__ [1 2 3] 2) '(1 1 2 2 3 3)))
  (assert (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
  (assert (= (__ [4 5 6] 1) '(4 5 6)))
  (assert (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
  (assert (= (__ [44 33] 2) [44 44 33 33])))

;http://www.4clojure.com/problem/34
;Implement Range
(let [__ (fn[lo hi](take-while #(< % hi) (iterate inc lo)))]
  (assert (= (__ 1 4) '(1 2 3)))
  (assert (= (__ -2 2) '(-2 -1 0 1)))
  (assert (= (__ 5 8) '(5 6 7))))

;http://www.4clojure.com/problem/35
;Local bindings
(let [__ 7]
  (assert (= __ (let [x 5] (+ 2 x))))
  (assert (= __ (let [x 3, y 10] (- y x))))
  (assert (= __ (let [x 21] (let [y 3] (/ x y))))))

;http://www.4clojure.com/problem/36
;Let it Be
(assert (= 10 (let[x 7 y 3 z 1] (+ x y))))
(assert (= 4 (let[x 7 y 3 z 1] (+ y z))))
(assert (= 1 (let[x 7 y 3 z 1] z)))

;http://www.4clojure.com/problem/37
;Regular Expressions
(let [__ "ABC"]
  (assert (= __ (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))))

;http://www.4clojure.com/problem/38
;Maximum value
(let [__ (fn[f & r](reduce (fn[a b](if (> a b) a b)) f r))]
  (assert (= (__ 1 8 3 4) 8))
  (assert (= (__ 30 20) 30))
  (assert (= (__ 45 67 11) 67)))

;http://www.4clojure.com/problem/39
;Interleave Two Seqs
(let [__ #(flatten (map vector %1 %2))]
  (assert (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
  (assert (= (__ [1 2] [3 4 5 6]) '(1 3 2 4)))
  (assert (= (__ [1 2 3 4] [5]) [1 5]))
  (assert (= (__ [30 20] [25 15]) [30 25 20 15])))

;http://www.4clojure.com/problem/40
;Interpose a Seq
(let [__ #(butlast (interleave %2 (repeat %1)))]
  (assert (= (__ 0 [1 2 3]) [1 0 2 0 3]))
  (assert (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three"))
  (assert (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])))
