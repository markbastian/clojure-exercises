(ns clojure4.x061-080)

;http://www.4clojure.com/problem/61
;Map Construction
(let [__ #(apply hash-map (interleave %1 %2))]
  (assert (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (assert (= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (assert (= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

;http://www.4clojure.com/problem/62
;Re-implement Iterate
(let [__ (fn iter [f x] (lazy-seq (cons x (iter f (f x)))))]
  (assert (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16]))
  (assert (= (take 100 (__ inc 0)) (take 100 (range))))
  (assert (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

;http://www.4clojure.com/problem/63
;Group a Sequence
(let [__ (fn[f s] (reduce (fn[m i]
                            (let[k (f i)]
                              (assoc m k (if (m k) (conj (m k) i) [i]))))
                          {} s))]
  (assert (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (assert (= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
             {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (assert (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
             {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

;http://www.4clojure.com/problem/64
;Intro to Reduce
(let [__ +]
  (assert (= 15 (reduce __ [1 2 3 4 5])))
  (assert (=  0 (reduce __ [])))
  (assert (=  6 (reduce __ 1 [2 3]))))

;http://www.4clojure.com/problem/65
;Black Box Testing
(let [__ #(let [x (conj % {:b 1} {:a 1} {:a 1})]
           (cond
             (= 1 (get x :a)) :map
             (= {:a 1} (get x {:a 1})) :set
             (= {:a 1} (first x)) :list
             (= {:a 1} (last x)) :vector))]
  (assert (= :map (__ {:a 1, :b 2})))
  (assert (= :list (__ (range (rand-int 20)))))
  (assert (= :vector (__ [1 2 3 4 5 6])))
  (assert (= :set (__ #{10 (rand-int 5)})))
  (assert (= [:map :set :vector :list] (map __ [{} #{} [] ()]))))

;http://www.4clojure.com/problem/66
;Greatest Common Divisor
(let [__ (fn[a b] (let[lo (min a b)]
                    (some #(when (= 0 (mod a %) (mod b %)) %) (range lo 0 -1))))]
  (assert (= (__ 2 4) 2))
  (assert (= (__ 10 5) 5))
  (assert (= (__ 5 7) 1))
  (assert (= (__ 1023 858) 33)))

;http://www.4clojure.com/problem/67
;Prime Numbers
;Note - This implementation uses a prime sieve and will fail when n exceeds the internal sieve max.
(let [__ (fn [n] (take n ((fn [maxprime]
                            (loop[[f & r] (range 2 maxprime) primes []]
                              (if f
                                (recur (filter #(not= 0 (mod % f)) r) (conj primes f))
                                primes))) 1000)))]
  (assert (= (__ 2) [2 3]))
  (assert (= (__ 5) [2 3 5 7 11]))
  (assert (= (last (__ 100)) 541)))

;http://www.4clojure.com/problem/68
;Recurring Theme
(let [__ [7 6 5 4 3]]
  (assert (= __
             (loop [x 5 result []]
               (if (> x 0)
                 (recur (dec x) (conj result (+ 2 x)))
                 result)))))

;http://www.4clojure.com/problem/69
;Merge with a Function
(let [__ (fn [op & args]
           (reduce
             (fn [a b]
               (into (into a b) (for [[k v] a :let [u (b k)] :when u] [k (op v u)])))
             {} args))]
  (assert (= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
             {:a 4, :b 6, :c 20}))
  (assert (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
             {1 7, 2 10, 3 15}))
  (assert (= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
             {:a [3 4 5], :b [6 7], :c [8 9]})))

;http://www.4clojure.com/problem/70
;Word Sorting
(let [__ (fn[w]
           (sort #(compare
                   (clojure.string/lower-case %1)
                   (clojure.string/lower-case %2))
                 (clojure.string/split w #"[^A-Za-z]+")))]
  (assert (= (__  "Have a nice day.")
             ["a" "day" "Have" "nice"]))
  (assert (= (__  "Clojure is a fun language!")
             ["a" "Clojure" "fun" "is" "language"]))
  (assert (= (__  "Fools fall for foolish follies.")
             ["fall" "follies" "foolish" "Fools" "for"])))

;http://www.4clojure.com/problem/71
;Rearranging Code: ->
(let [__ last]
  (assert (= (__ (sort (rest (reverse [2 5 4 1 3 6]))))
             (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (__))
             5)))

;http://www.4clojure.com/problem/72
;Rearranging Code: ->>
(assert (= (apply + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
           (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (apply +))
           11))

;http://www.4clojure.com/problem/73
;Analyze a Tic-Tac-Toe Board
(let [__ #(first
           (some #{[:x :x :x][:o :o :o]}
                 (-> %
                     (into (apply map vector %))
                     (conj (map nth % (range))
                           (map nth (rseq %) (range))))))]
  (assert (= nil (__ [[:e :e :e]
                      [:e :e :e]
                      [:e :e :e]])))
  (assert (= :x (__ [[:x :e :o]
                     [:x :e :e]
                     [:x :e :o]])))
  (assert (= :o (__ [[:e :x :e]
                     [:o :o :o]
                     [:x :e :x]])))
  (assert (= nil (__ [[:x :e :o]
                      [:x :x :e]
                      [:o :x :o]])))
  (assert (= :x (__ [[:x :e :e]
                     [:o :x :e]
                     [:o :e :x]])))
  (assert (= :o (__ [[:x :e :o]
                     [:x :o :e]
                     [:o :e :x]])))
  (assert (= nil (__ [[:x :o :x]
                      [:x :o :x]
                      [:o :x :o]]))))

;http://www.4clojure.com/problem/74
;Filter Perfect Squares
(let [__ (fn[s]
           (->> (clojure.string/split s #",")
                (map read-string)
                (filter #(zero? (mod (Math/sqrt %) 1)))
                (clojure.string/join ",")))]
  (assert (= (__ "4,5,6,7,8,9") "4,9"))
  (assert (= (__ "15,16,25,36,37") "16,25,36")))

;http://www.4clojure.com/problem/75
;Euler's Totient Function
(let [__ (fn[n]
           (if (= n 1)
             1
             (let[prime-factors
                  (loop [[f & r] (range 2 n) primes [1]]
                    (if f
                      (recur (filter #(pos? (mod % f)) r) (conj primes f))
                      (filter #(zero? (mod n %)) primes)))
                  coprimes (remove (fn [c] (some #(zero? (mod c %)) (rest prime-factors))) (range 1 n))]
               (count coprimes))))]
  (assert (= (__ 1) 1))
  (assert (= (__ 10) (count '(1 3 7 9)) 4))
  (assert (= (__ 40) 16))
  (assert (= (__ 99) 60)))

;http://www.4clojure.com/problem/76
;Intro to Trampoline
(let [__ [1 3 5 7 9 11]]
  (assert (= __
             (letfn
               [(foo [x y] #(bar (conj x y) y))
                (bar [x y] (if (> (last x) 10)
                             x
                             #(foo x (+ 2 y))))]
               (trampoline foo [] 1)))))

;http://www.4clojure.com/problem/77
;Anagram Finder
(let [__ (fn[c]
           (->> c
                (group-by frequencies)
                (map (comp (partial apply hash-set) second))
                (filter #(-> % count dec pos?))
                (apply hash-set)))]
  (assert (= (__ ["meat" "mat" "team" "mate" "eat"])
             #{#{"meat" "team" "mate"}}))
  (assert (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
             #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})))

;http://www.4clojure.com/problem/78
;Reimplement Trampoline
(let [__ (fn [f args]
           (loop [res (f args)]
             (if (fn? res) (recur (res)) res)))]
  (assert (= (letfn [(triple [x] #(sub-two (* 3 x)))
                     (sub-two [x] #(stop?(- x 2)))
                     (stop? [x] (if (> x 50) x #(triple x)))]
               (__ triple 2))
             82))
  (assert (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                     (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
               (map (partial __ my-even?) (range 6)))
             [true false true false true false])))

;http://www.4clojure.com/problem/79
;Triangle Minimal Path
(let [__ (fn minpath [t]
           (letfn [(nbrs [[i j]] [[(inc i) j] [(inc i) (inc j)]])
                   (spread [paths] (vec (for [path paths n (nbrs (peek path))] (conj path n))))]
             (->> (nth (iterate spread [[[0 0]]]) (dec (count t)))
                  (map (fn [path](map #(get-in (vec t) %) path)))
                  (map #(reduce + %))
                  (apply min))))]
  (assert (= 7 (__ '([1]
                      [2 4]
                      [5 1 4]
                      [2 3 4 5]))) ; 1->2->1->3
    )
  (assert (= 20 (__ '([3]
                       [2 4]
                       [1 9 3]
                       [9 9 2 4]
                       [4 6 6 7 8]
                       [5 7 3 5 1 4]))) ; 3->4->3->2->7->1
    ))

;http://www.4clojure.com/problem/80
;Perfect Numbers
(let [__ (fn [n]
           (->> (range (/ n 2))
                (map inc)
                (filter #(zero? (mod n %)))
                (reduce +)
                (= n)))]
  (assert (= (__ 6) true))
  (assert (= (__ 7) false))
  (assert (= (__ 496) true))
  (assert (= (__ 500) false))
  (assert (= (__ 8128) true)))