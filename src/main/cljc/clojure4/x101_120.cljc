(ns clojure4.x101-120)

;http://www.4clojure.com/problem/101
;Levenshtein Distance
(let [__
      (fn lev [a b]
        (letfn [(l [m [i j :as ij]]
                  (if (zero? (min i j))
                    (max i j)
                    (min (inc (get-in m [(dec i) j]))
                         (inc (get-in m [i (dec j)]))
                         (+ (get-in m [(dec i) (dec j)])
                            (if (= (get a (dec i)) (get b (dec j))) 0 1)))))]
          (let [grid (reduce
                       (fn [m ij] (assoc-in m ij (l m ij)))
                       (vec (repeat (count b) []))
                       (for [i (range (inc (count a))) j (range (inc (count b)))] [i j]))]
            (get-in grid [(count a) (count b)]))))]
  (assert (= (__ "kitten" "sitting") 3))
  (assert (= (__ "closure" "clojure") (__ "clojure" "closure") 1))
  (assert (= (__ "xyx" "xyyyx") 2))
  (assert (= (__ "" "123456") 6))
  (assert (= (__ "Clojure" "Clojure") (__ "" "") (__ [] []) 0))
  (assert (= (__ [1 2 3 4] [0 2 3 4 5]) 2))
  (assert (= (__ '(:a :b :c :d) '(:a :d)) 2))
  (assert (= (__ "ttttattttctg" "tcaaccctaccat") 10))
  (assert (= (__ "gaattctaatctc" "caaacaaaaaattt") 9)))

;indexed-based recursive version that is inefficient
#_(defn levenshtein [a b]
  (letfn [(lev [i j]
            (cond
              (zero? i) j
              (zero? j) i
              :default (min (inc (lev (dec i) j))
                            (inc (lev i (dec j)))
                            (+ (lev (dec i) (dec j))
                               (if (= (get a (dec i)) (get b (dec j))) 0 1)))))]
    (lev (count a) (count b))))

;seq-based recursive version that is inefficient
#_(defn levenshtein [a b]
  (cond
    (empty? a) (count b)
    (empty? b) (count a)
    :default (min (inc (levenshtein (butlast a) b))
                  (inc (levenshtein a (butlast b)))
                  (+ (levenshtein (butlast a) (butlast b))
                     (if (= (last a) (last b)) 0 1)))))

;http://www.4clojure.com/problem/102
;102intoCamelCase
(let [__ (fn[s](clojure.string/replace
                 s
                 #"-(.)"
                 #(clojure.string/upper-case (second %))))]
  (assert (= (__ "something") "something"))
  (assert (= (__ "multi-word-key") "multiWordKey"))
  (assert (= (__ "leaveMeAlone") "leaveMeAlone")))

;http://www.4clojure.com/problem/103
;Generating k-combinations
(let [__ (fn[n opts]
           (nth (iterate
                  #(into #{} (for [s % o opts :when (not (s o))] (conj s o)))
                  #{#{}}) n))]
  (assert (= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}}))
  (assert (= (__ 10 #{4 5 6}) #{}))
  (assert (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
  (assert (= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                   #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
  (assert (= (__ 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
  (assert (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})))

;http://www.4clojure.com/problem/104
;Write Roman Numerals
(let [__ (fn [d]
           (let [m {0 ["" "" "" ""]
                    1 ["I" "X" "C" "M"]
                    2 ["II" "XX" "CC" "MM"]
                    3 ["III" "XXX" "CCC" "MMM"]
                    4 ["IV" "XL" "CD" "MMMM"]
                    5 ["V" "L" "D" "MMMMM"]
                    6 ["VI" "LX" "DC" "MMMMMM"]
                    7 ["VII" "LXX" "DCC" "MMMMMMM"]
                    8 ["VIII" "LXXX" "DCCC" "MMMMMMMM"]
                    9 ["IX" "XC" "CM" "MMMMMMMMM"]}]
             (loop [q d i 0 res ()]
               (if (pos? q)
                 (recur (quot q 10) (inc i) (conj res (get-in m [(rem q 10) i])))
                 (apply str res)))))]
  (assert (= "I" (__ 1)))
  (assert (= "XXX" (__ 30)))
  (assert (= "IV" (__ 4)))
  (assert (= "CXL" (__ 140)))
  (assert (= "DCCCXXVII" (__ 827)))
  (assert (= "MMMCMXCIX" (__ 3999)))
  (assert (= "XLVIII" (__ 48))))

;http://www.4clojure.com/problem/105
;Identify keys and values
(let [__ #(loop [[k & r] % c nil res {}]
            (cond
              (nil? k) res
              (keyword? k) (recur r k (assoc res k []))
              :default (recur r c (update-in res [c] conj k))))]
  (assert (= {} (__ [])))
  (assert (= {:a [1]} (__ [:a 1])))
  (assert (= {:a [1], :b [2]} (__ [:a 1, :b 2])))
  (assert (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))))

;http://www.4clojure.com/problem/106
;Number Maze
(let [__ (fn[a b]
           (let[nbrs (fn[x]
                       (if (zero? (rem x 2))
                         [(* 2 x) (/ x 2) (+ x 2)]
                         [(* 2 x) (+ x 2)]))]
             (inc (count (take-while #(not (% b)) (iterate #(reduce into #{} (map nbrs %)) #{a}))))))]
  (assert (= 1 (__ 1 1)))
  (assert (= 3 (__ 3 12)))
  (assert (= 3 (__ 12 3)))
  (assert (= 3 (__ 5 9)))
  (assert (= 9 (__ 9 2)))
  (assert (= 5 (__ 9 12))))

;http://www.4clojure.com/problem/107
;Simple closures
(let [__ (fn[n](fn[x](reduce * (repeat n x))))]
  (assert (= 256 ((__ 2) 16) ((__ 8) 2)))
  (assert (= [1 8 27 64] (map (__ 3) [1 2 3 4])))
  (assert (= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4]))))

;http://www.4clojure.com/problem/108
;Lazy Searching
(let [__ (fn[& seqs]
           (loop[s (vec seqs)]
             (let [firsts (map-indexed (fn [i v] [i (first v)]) s)
                   [i v]  (apply min-key second firsts)]
               (if (apply = (map second firsts))
                 v
                 (recur (update-in s [i] rest))))))]
  (assert (= 3 (__ [3 4 5])))
  (assert (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
  (assert (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13])))
  (assert (= 64 (__ (map #(* % % %) (range)) ;; perfect cubes
                    (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                    (iterate inc 20))) ;; at least as large as 20
    ))

;http://www.4clojure.com/problem/109 - Does not exist

;http://www.4clojure.com/problem/110
;Sequence of pronunciations
(let [__ (fn [ic]
           (rest (iterate
                   #(loop [s % res []]
                      (if (first s)
                        (let [[f r] (split-with #{(first s)} s)]
                          (recur r (conj res (count f) (first s))))
                        res))
                   ic)))]
  (assert (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1]))))
  (assert (= [3 1 2 4] (first (__ [1 1 1 4 4]))))
  (assert (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6)))
  (assert (= 338 (count (nth (__ [3 2]) 15)))))

;http://www.4clojure.com/problem/111
;Crossword puzzle
(let [__ (letfn [(valid-row [goal row]
                   (loop [[a b] (split-with (complement #{\#}) (remove #{\space} row))]
                     (cond
                       (and (= (count a) (count goal))
                            (every? true? (map (fn [a b] (or (= a b) (= b \_))) goal a))) true
                       (empty? b) false
                       :default (recur (split-with (complement #{\#}) (rest b))))))]
           (fn [word puzzle]
             (let [v (partial valid-row word)]
               (true? (or (some v puzzle)
                          (some v (apply map vector puzzle)))))))]
  (assert (= true  (__ "the" ["_ # _ _ e"])))
  (assert (= false (__ "the" ["c _ _ _"
                              "d _ # e"
                              "r y _ _"])))
  (assert (= true  (__ "joy" ["c _ _ _"
                              "d _ # e"
                              "r y _ _"])))
  (assert (= false (__ "joy" ["c o n j"
                              "_ _ y _"
                              "r _ _ #"])))
  (assert (= true  (__ "clojure" ["_ _ _ # j o y"
                                  "_ _ o _ _ _ _"
                                  "_ _ f _ # _ _"]))))

;http://www.4clojure.com/problem/112
;Sequs Horribilis
(let [__
      (fn sequs
        ([[f & r] coll tot mx]
         (cond
           (nil? f) coll
           (number? f) (let [x (+ tot f)]
                         (if (> x mx) coll (sequs r (conj coll f) x mx)))
           :default (let [t (sequs f [] 0 (- mx tot))
                          x (reduce + (flatten t))]
                      (sequs r (conj coll t) (+ tot x) mx))))
        ([mx s] (sequs s [] 0 mx)))]
  (assert (=  (__ 10 [1 2 [3 [4 5] 6] 7])
              '(1 2 (3 (4)))))
  (assert (=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
              '(1 2 (3 (4 (5 (6 (7))))))))
  (assert (=  (__ 9 (range))
              '(0 1 2 3)))
  (assert (=  (__ 1 [[[[[1]]]]])
              '(((((1)))))))
  (assert (=  (__ 0 [1 2 [3 [4 5] 6] 7])
              '()))
  (assert (=  (__ 0 [0 0 [0 [0]]])
              '(0 0 (0 (0)))))
  (assert (=  (__ 2 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
              '(-10 (1 (2 3 (4)))))))

;http://www.4clojure.com/problem/113
;Making Data Dance
(let [__
      (fn [& s]
        (reify
          java.lang.Object
          (toString [_] (apply str (interpose ", " (sort s))))
          clojure.lang.Seqable
          (seq [_] (seq (loop [[f & r] s visited #{} res []]
                          (cond
                            (nil? f) res
                            (visited f) (recur r visited res)
                            :default (recur r (conj visited f) (conj res f))))))))]
  (assert (= "1, 2, 3" (str (__ 2 1 3))))
  (assert (= '(2 1 3) (seq (__ 2 1 3))))
  (assert (= '(2 1 3) (seq (__ 2 1 3 3 1 2))))
  (assert (= '(1) (seq (apply __ (repeat 5 1)))))
  (assert (= "1, 1, 1, 1, 1" (str (apply __ (repeat 5 1)))))
  (assert (and (= nil (seq (__)))
               (=  "" (str (__))))))

;http://www.4clojure.com/problem/114
;Global take-while
(let [__ (fn gtw [n p [f & r]]
           (when f
             (let [ctr (if (p f) (dec n) n)]
               (when (pos? ctr)
                 (lazy-seq (cons f (gtw ctr p r)))))))]
  (assert (= [2 3 5 7 11 13]
             (__ 4 #(= 2 (mod % 3))
                 [2 3 5 7 11 13 17 19 23])))
  (assert (= ["this" "is" "a" "sentence"]
             (__ 3 #(some #{\i} %)
                 ["this" "is" "a" "sentence" "i" "wrote"])))
  (assert (= ["this" "is"]
             (__ 1 #{"a"}
                 ["this" "is" "a" "sentence" "i" "wrote"]))))

;http://www.4clojure.com/problem/115
;The Balance of N
(let [__ (fn [n]
           (let [digits (loop [q [(mod n 10)] r (quot n 10)]
                          (if (pos? r) (recur (conj q (mod r 10)) (quot r 10)) q))
                 c (quot (count digits) 2)]
             (= (apply + (take c digits))
                (apply + (take c (rseq digits))))))]
  (assert (= true (__ 11)))
  (assert (= true (__ 121)))
  (assert (= false (__ 123)))
  (assert (= true (__ 0)))
  (assert (= false (__ 88099)))
  (assert (= true (__ 89098)))
  (assert (= true (__ 89089)))
  (assert (= (take 20 (filter __ (range)))
             [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]) ))

;http://www.4clojure.com/problem/116
;Prime Sandwich
(let [__
      (fn[num]
        (let [prime-seq
              (->>
                (iterate
                  (fn [primes]
                    (let [prime? (fn [n primes] (->> primes (filter #(<= (* % %) n)) (map #(mod n %)) (not-any? zero?)))]
                      (conj primes (some #(let [n (+ (peek primes) % 1)] (when (prime? n primes) n)) (range)))))
                  [2])
                (map peek))
              [l c r] (->> prime-seq (partition 3 1) (filter (fn [[_ x _]] (>= x num))) first)]
          (= num c (/ (+ l r) 2))))]
  (assert (= false (__ 4)))
  (assert (= true (__ 563)))
  (assert (= 1103 (nth (filter __ (range)) 15))))

;http://www.4clojure.com/problem/117
;For Science!
(let [__
      (fn [m]
        (letfn [(neighbors [[i j]]
                  (map vector
                       ((juxt inc identity dec identity) i)
                       ((juxt identity inc identity dec) j)))]
          (let [start (first (for [r (range (count m)) c (range (count (m r))) :when (#{\M} (get-in m [r c]))] [r c]))
                finish (first (for [r (range (count m)) c (range (count (m r))) :when (#{\C} (get-in m [r c]))] [r c]))
                possible (apply hash-set (for [r (range (count m)) c (range (count (m r))) :when (#{\space \C} (get-in m [r c]))] [r c]))]
            (loop [visited #{start}]
              (let [n (into visited (filter possible (mapcat neighbors visited)))]
                (cond
                  (n finish) true
                  (= visited n) false
                  :default (recur n)))))))]
  (assert (= true  (__ ["M   C"])))
  (assert (= false (__ ["M # C"])))
  (assert (= true  (__ ["#######"
                        "#     #"
                        "#  #  #"
                        "#M # C#"
                        "#######"])))
  (assert (= false (__ ["########"
                        "#M  #  #"
                        "#   #  #"
                        "# # #  #"
                        "#   #  #"
                        "#  #   #"
                        "#  # # #"
                        "#  #   #"
                        "#  #  C#"
                        "########"])))
  (assert (= false (__ ["M     "
                        "      "
                        "      "
                        "      "
                        "    ##"
                        "    #C"])))
  (assert (= true  (__ ["C######"
                        " #     "
                        " #   # "
                        " #   #M"
                        "     # "])))
  (assert (= true  (__ ["C# # # #"
                        "        "
                        "# # # # "
                        "        "
                        " # # # #"
                        "        "
                        "# # # #M"]))))

;http://www.4clojure.com/problem/118
;
(let [__ (fn mp[f [s & r]] (if s (lazy-seq (cons (f s) (mp f r))) nil))]
  (assert (= [3 4 5 6 7]
             (__ inc [2 3 4 5 6])))
  (assert (= (repeat 10 nil)
             (__ (fn [_] nil) (range 10))))
  (assert (= [1000000 1000001]
             (->> (__ inc (range))
                  (drop (dec 1000000))
                  (take 2)))))

;http://www.4clojure.com/problem/119
;Win at Tic-Tac-Toe
(let [__
      (fn[p b]
        (into #{}
              (let [f #(first
                         (some #{[p p p]}
                               (-> %
                                   (into (apply map vector %))
                                   (conj (map nth % (range))
                                         (map nth (rseq %) (range))))))]
                (for [i (range 3) j (range 3)
                      :let [cp (get-in b [i j])
                            b (assoc-in b [i j] p)]
                      :when (and (= cp :e) (= p (f b)))]
                  [i j]))))]
  (assert (= (__ :x [[:o :e :e]
                     [:o :x :o]
                     [:x :x :e]])
             #{[2 2] [0 1] [0 2]}))
  (assert (= (__ :x [[:x :o :o]
                     [:x :x :e]
                     [:e :o :e]])
             #{[2 2] [1 2] [2 0]}))
  (assert (= (__ :x [[:x :e :x]
                     [:o :x :o]
                     [:e :o :e]])
             #{[2 2] [0 1] [2 0]}))
  (assert (= (__ :x [[:x :x :o]
                     [:e :e :e]
                     [:e :e :e]])
             #{}))
  (assert (= (__ :o [[:x :x :o]
                     [:o :e :o]
                     [:x :e :e]])
             #{[2 2] [1 1]})))

;http://www.4clojure.com/problem/120
;Sum of square of digits
(let [__
(fn[r]
  (->> r
       (map
         (fn [n]
           (< n
              (->> (loop [q [(mod n 10)] r (quot n 10)]
                     (if (pos? r) (recur (conj q (mod r 10)) (quot r 10)) q))
                   (map (fn [x] (* x x)))
                   (reduce +)))))
       (filter identity)
       count))]
  (assert (= 8 (__ (range 10))))
  (assert (= 19 (__ (range 30))))
  (assert (= 50 (__ (range 100))))
  (assert (= 50 (__ (range 1000)))))

