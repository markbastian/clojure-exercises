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
;;http://www.4clojure.com/problem/84
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
;Note: update-in and not= nil were used in place of update and some? due to lack of 4clojure support.
(let [__ (letfn [(expand [paths]
                   (for [{:keys [path remaining-edges] :as m} paths
                         :let [f #{(peek path)}]
                         unvisited (filter (fn [edge] (some f edge)) remaining-edges)
                         :let [n (first (remove f unvisited))
                               [a b] (split-with (complement #{unvisited}) remaining-edges)]]
                     (-> m
                         (update-in [:path] conj n)
                         (assoc :remaining-edges (into a (rest b))))))]
           (fn [edges]
             (let [vertices (distinct (flatten edges))
                   starts (map (fn [p] {:path [p] :remaining-edges edges}) vertices)]
               (not= nil (not-empty (nth (iterate expand starts) (count edges)))))))]
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

;http://www.4clojure.com/problem/91
;Graph Connectivity
;Note: 4Clojure requires distinct elements when calling hash-set
(let [__
      (fn [edges]
        (let [vertices (apply hash-set (distinct (flatten (seq edges))))]
          (loop [visited #{(first vertices)} remaining edges]
            (if (= visited vertices)
              true
              (let [{:keys [can-visit later]} (group-by #(if (some visited %) :can-visit :later) remaining)]
                (if (empty? can-visit)
                  false
                  (recur (apply conj visited (flatten can-visit)) (apply hash-set later))))))))]
  (assert (= true (__ #{[:a :a]})))
  (assert (= true (__ #{[:a :b]})))
  (assert (= false (__ #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4]})))
  (assert (= true (__ #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4] [3 4]})))
  (assert (= false (__ #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e]})))
  (assert (= true (__ #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e] [:x :a]}))))

;http://www.4clojure.com/problem/92
;Read Roman numerals
(let [__ #(loop [[f & r] (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} %) s 0]
           (if r
             (if (< f (first r))
               (recur r (- s f))
               (recur r (+ s f)))
             (+ s f)))]
  (assert (= 14 (__ "XIV")))
  (assert (= 827 (__ "DCCCXXVII")))
  (assert (= 827 (__ "DCCCXXVII")))
  (assert (= 48 (__ "XLVIII"))))

;http://www.4clojure.com/problem/93
;Partially Flatten a Sequence
(let [__ (fn parflat [s]
           (if (not-any? coll? s) [s] (mapcat parflat s)))]
  (assert (= (__ [["Do"] ["Nothing"]])
             [["Do"] ["Nothing"]]))
  (assert (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
             [[:a :b] [:c :d] [:e :f]]))
  (assert (= (__ '((1 2)((3 4)((((5 6)))))))
             '((1 2)(3 4)(5 6)))))

;http://www.4clojure.com/problem/94
;Game of Life
(let [__ #(let [nx (juxt inc inc identity dec dec dec identity inc)
                ny (juxt identity inc inc inc identity dec dec dec)]
           (for [i (range (count %))]
             (apply str
                    (for [j (range (count %))
                          :let [curr (get-in % [i j])
                                live-neighbors ((frequencies (map (comp (partial get-in %) vector) (nx i) (ny j))) \#)]]
                      (case live-neighbors
                        3 \#
                        2 (if (= curr \#) \# \space)
                        \space)))))]
  (assert (= (__ ["      "
                  " ##   "
                  " ##   "
                  "   ## "
                  "   ## "
                  "      "])
             ["      "
              " ##   "
              " #    "
              "    # "
              "   ## "
              "      "]))
  (assert (= (__ ["     "
                  "     "
                  " ### "
                  "     "
                  "     "])
             ["     "
              "  #  "
              "  #  "
              "  #  "
              "     "]))
  (assert (= (__ ["      "
                  "      "
                  "  ### "
                  " ###  "
                  "      "
                  "      "])
             ["      "
              "   #  "
              " #  # "
              " #  # "
              "  #   "
              "      "])))

;http://www.4clojure.com/problem/95
;To Tree, or not to Tree
(let [__ (fn tree? [m]
           (if (and (coll? m) (= 3 (count m)))
             (let[[l c r] m] (and l (tree? c) (tree? r)))
             (nil? m)))]
  (assert (= (__ '(:a (:b nil nil) nil))
             true))
  (assert (= (__ '(:a (:b nil nil)))
             false))
  (assert (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
             true))
  (assert (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
             false))
  (assert (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
             true))
  (assert (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
             false))
  (assert (= (__ '(:a nil ()))
             false)))

;http://www.4clojure.com/problem/96
;Beauty is Symmetry
(let [__ (fn [[_ b c]]
           (= b
              ((fn rev [[k l r :as arg]]
                 (when arg [k (rev r) (rev l)])) c)))]
  (assert (= (__ '(:a (:b nil nil) (:b nil nil))) true))
  (assert (= (__ '(:a (:b nil nil) nil)) false))
  (assert (= (__ '(:a (:b nil nil) (:c nil nil))) false))
  (assert (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                  [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
             true))
  (assert (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                  [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
             false))
  (assert (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                  [2 [3 nil [4 [6 nil nil] nil]] nil]])
             false)))

;http://www.4clojure.com/problem/97
;Pascal's Triangle
(let [__ (fn [n]
           (nth (iterate
                  (fn[p]
                    (if (empty? p)
                      [1]
                      (conj (into [1] (map (fn [[a b]] (+ a b)) (partition 2 1 p))) 1)))
                  []) n))]
  (assert (= (__ 1) [1]))
  (assert (= (map __ (range 1 6))
             [[1]
              [1 1]
              [1 2 1]
              [1 3 3 1]
              [1 4 6 4 1]]))
  (assert (= (__ 11)
             [1 10 45 120 210 252 210 120 45 10 1])))

;http://www.4clojure.com/problem/98
;Equivalence Classes
(let [__ (fn[f d]
           (->> (zipmap d (map f d))
                (group-by second)
                vals
                (map #(into #{} (map first %)))
                (into #{})))]
  (assert (= (__ #(* % %) #{-2 -1 0 1 2})
             #{#{0} #{1 -1} #{2 -2}}))
  (assert (= (__ #(rem % 3) #{0 1 2 3 4 5 })
             #{#{0 3} #{1 4} #{2 5}}))
  (assert (= (__ identity #{0 1 2 3 4})
             #{#{0} #{1} #{2} #{3} #{4}}))
  (assert (= (__ (constantly true) #{0 1 2 3 4})
             #{#{0 1 2 3 4}})))

;http://www.4clojure.com/problem/99
;Product Digits
(let [__ #(let[p (* %1 %2)]
           (loop[r (quot p 10) res [(mod p 10)]]
             (if (pos? r)
               (recur (quot r 10) (conj res (mod r 10)))
               (rseq res))))]
  (assert (= (__ 1 1) [1]))
  (assert (= (__ 99 9) [8 9 1]))
  (assert (= (__ 999 99) [9 8 9 0 1])))

;http://www.4clojure.com/problem/100
;Least Common Multiple
(let [__ (fn [f & args]
           (some (fn [n] (when (every? zero? (map #(mod n %) args)) n))
                 (map #(* f (inc %)) (range))))]
  (assert (== (__ 2 3) 6))
  (assert (== (__ 5 3 7) 105))
  (assert (== (__ 1/3 2/5) 2))
  (assert (== (__ 3/4 1/6) 3/2))
  (assert (== (__ 7 5/7 2 3/5) 210)))

;;http://www.4clojure.com/problem/
;;
;(let [__ false]
;  (assert )
;  (assert )
;  (assert ))
