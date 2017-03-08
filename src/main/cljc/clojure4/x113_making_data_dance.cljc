(ns clojure4.x113-making-data-dance
  [:require [clojure.string :as cs]])

;http://www.4clojure.com/problem/113
;Making Data Dance

(def __
  (fn [& s]
    (reify
      java.lang.Object
      (toString [_] (apply str (interpose ", " (sort s))))
      clojure.lang.Seqable
      (seq [_] (seq (loop [[f & r] s visited #{} res []]
                      (cond
                        (nil? f) res
                        (visited f) (recur r visited res)
                        :default (recur r (conj visited f) (conj res f)))))))))

(assert (= "1, 2, 3" (str (__ 2 1 3))))

(assert (= '(2 1 3) (seq (__ 2 1 3))))

(assert (= '(2 1 3) (seq (__ 2 1 3 3 1 2))))

(assert (= '(1) (seq (apply __ (repeat 5 1)))))

(assert (= "1, 1, 1, 1, 1" (str (apply __ (repeat 5 1)))))

(assert (and (= nil (seq (__)))
             (=  "" (str (__)))))