(ns clojure4.x001-020)

;http://www.4clojure.com/problem/1
;Nothing but the Truth
(let [__ true]
  (assert (= __ true)))

;http://www.4clojure.com/problem/2
;Simple Math
(let [__ 4]
  (assert (= (- 10 (* 2 3)) __)))

;http://www.4clojure.com/problem/3
;Intro to Strings
(let [__ "HELLO WORLD"]
  (assert (= __ (.toUpperCase "hello world"))))

;http://www.4clojure.com/problem/4
;Intro to Lists
(let [__ (list :a :b :c)]
  (assert (= __ '(:a :b :c))))

;http://www.4clojure.com/problem/4
;Intro to Lists
(let [__ (list :a :b :c)]
  (assert (= __ '(:a :b :c))))

;http://www.4clojure.com/problem/5
;Lists: conj
(let [__ '(1 2 3 4)]
  (assert (= __ (conj '(2 3 4) 1)))
  (assert (= __ (conj '(3 4) 2 1))))

;http://www.4clojure.com/problem/6
;Intro to Vectors
(let [__ [:a :b :c]]
  (assert (= __ (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))))

;http://www.4clojure.com/problem/7
;Vectors: conj
(let [__ [1 2 3 4]]
  (assert (= __ (conj [1 2 3] 4)))
  (assert (= __ (conj [1 2] 3 4))))

;http://www.4clojure.com/problem/8
;Intro to Sets
(let [__ #{:a :b :c :d}]
  (assert (= __ (set '(:a :a :b :c :c :c :c :d :d))))
  (assert (= __ (clojure.set/union #{:a :b :c} #{:b :c :d}))))

;http://www.4clojure.com/problem/9
;Sets: conj
(let [__ 2]
  (assert (= #{1 2 3 4} (conj #{1 4 3} __))))

;http://www.4clojure.com/problem/10
;Intro to Maps
(let [__ 20]
  (assert (= __ ((hash-map :a 10, :b 20, :c 30) :b)))
  (assert (= __ (:b {:a 10, :b 20, :c 30}))))

;http://www.4clojure.com/problem/11
;Maps: conj
(let [__ [:b 2]]
  (assert (= {:a 1, :b 2, :c 3} (conj {:a 1} __ [:c 3]))))

;http://www.4clojure.com/problem/12
;Intro to Sequences
(let [__ 3]
  (assert (= __ (first '(3 2 1))))
  (assert (= __ (second [2 3 4])))
  (assert (= __ (last (list 1 2 3)))))

;http://www.4clojure.com/problem/13
;Sequences: rest
(let [__ [20 30 40]]
  (assert (= __ (rest [10 20 30 40]))))

;http://www.4clojure.com/problem/14
;Intro to Functions
(let [__ 8]
  (assert (= __ ((fn add-five [x] (+ x 5)) 3)))
  (assert (= __ ((fn [x] (+ x 5)) 3)))
  (assert (= __ (#(+ % 5) 3)))
  (assert (= __ ((partial + 5) 3))))

;http://www.4clojure.com/problem/15
;Double Down
(let [__ #(* % 2)]
  (assert (= (__ 2) 4))
  (assert (= (__ 3) 6))
  (assert (= (__ 11) 22))
  (assert (= (__ 7) 14)))

;http://www.4clojure.com/problem/16
;Hello World
(let [__ #(str "Hello, " % "!")]
  (assert (= (__ "Dave") "Hello, Dave!"))
  (assert (= (__ "Jenn") "Hello, Jenn!"))
  (assert (= (__ "Rhea") "Hello, Rhea!")))

;http://www.4clojure.com/problem/17
;Sequences: map
(let [__ '(6 7 8)]
  (assert (= __ (map #(+ % 5) '(1 2 3)))))

;http://www.4clojure.com/problem/18
;Sequences: filter
(let [__ '(6 7)]
  (assert (= __ (filter #(> % 5) '(3 4 5 6 7)))))

;http://www.4clojure.com/problem/19
;Last Element
(let [__ #(first (reverse %))]
  (assert (= (__ [1 2 3 4 5]) 5))
  (assert (= (__ '(5 4 3)) 3))
  (assert (= (__ ["b" "c" "d"]) "d")))

;Alternate solution using composition
(let [__ (comp first reverse)]
  (assert (= (__ [1 2 3 4 5]) 5))
  (assert (= (__ '(5 4 3)) 3))
  (assert (= (__ ["b" "c" "d"]) "d")))

;http://www.4clojure.com/problem/20
;Penultimate Element
(let [__ #(-> % butlast last)]
  (assert (= (__ (list 1 2 3 4 5)) 4))
  (assert (= (__ ["a" "b" "c"]) "b"))
  (assert (= (__ [[1 2] [3 4]]) [1 2])))

