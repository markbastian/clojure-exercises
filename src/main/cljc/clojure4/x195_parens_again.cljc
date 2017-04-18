(ns clojure4.x195-parens-again)

(defn expand
  ([s l o c]
   (cond
     (>= (count s) l) [s]
     (= (- l (count s)) (- o c)) (str s (apply str (repeat (- o c) ")")))
     (> o c) [(expand (str s "(") l (inc o) c) (expand (str s ")") l o (inc c))]
     :default [(expand (str s "(") l (inc o) c)]))
  ([n] (set (flatten (expand "" (* 2 n) 0 0)))))

(def __
  (fn expand
    ([s l o c]
     (cond
       (>= (count s) l) [s]
       (= (- l (count s)) (- o c)) (str s (apply str (repeat (- o c) ")")))
       (> o c) [(expand (str s "(") l (inc o) c) (expand (str s ")") l o (inc c))]
       :default [(expand (str s "(") l (inc o) c)]))
    ([n] (set (flatten (expand "" (* 2 n) 0 0))))))

(assert (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (__ n)) [0 1 2])))
(assert (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (__ 3)))
(assert (= 16796 (count (__ 10))))
(assert (= (nth (sort (filter #(.contains ^String % "(()()()())") (__ 9))) 6) "(((()()()())(())))"))
(assert (= (nth (sort (__ 12)) 5000) "(((((()()()()()))))(()))"))