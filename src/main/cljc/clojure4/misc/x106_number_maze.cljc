(ns clojure4.misc.x106-number-maze
  (:import (clojure.lang PersistentQueue)))

;(defn nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (zero? (mod x 2)) (conj (/ x 2))))

;With filtering
(defn number-maze-0 [s g]
  (letfn [(nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (zero? (mod x 2)) (conj (/ x 2))))]
    (loop [v #{} q (conj PersistentQueue/EMPTY [s 0])]
      (let [[n i] (peek q)]
        (if (= n g)
          i
          (recur (conj v n) (into (pop q) (map (fn [t] [t (inc i)]) (remove v (nbrs n))))))))))

;Cond
(defn number-maze-1 [s g]
  (letfn [(nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (zero? (mod x 2)) (conj (/ x 2))))]
    (loop [v #{} q (conj PersistentQueue/EMPTY [s 0])]
      (let [[n i] (peek q)]
        (cond
          (= n g) i
          (v n) (recur v (pop q))
          :default (recur (conj v n) (into (pop q) (map (fn [t] [t (inc i)]) (nbrs n)))))))))

;Path + advanced destructuring
(defn number-maze-2 [s g]
  (letfn [(nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (zero? (mod x 2)) (conj (/ x 2))))]
    (loop [v {} [[n :as edge] :as q] (conj PersistentQueue/EMPTY [s :done])]
      (cond
        (v g) [(reverse (take-while (complement #{:done}) (iterate v g))) (count v)]
        (v n) (recur v (pop q))
        :default (recur (conj v edge) (into (pop q) (map (fn [x] [x n]) (nbrs n))))))))

(number-maze-2 7 43)

(defn expand [{v :visited [[n :as edge]] :queue :as m}]
  (letfn [(nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (zero? (mod x 2)) (conj (/ x 2))))]
    (cond-> (update m :queue pop) (not (v n))
            (->
              (update :visited conj edge)
              (update :queue into (map (fn [x] [x n]) (nbrs n)))))))

(defn number-maze-solver [start]
  (iterate expand {:visited {} :queue (conj PersistentQueue/EMPTY [start :done])}))

;(take 10 (iterate expand {:visited {} :queue (conj PersistentQueue/EMPTY [7 :done])}))

(defn solve-path [s g]
  (some
    (fn [{:keys [visited]}] (when (visited g) (reverse (take-while (complement #{:done}) (iterate visited g)))))
    (number-maze-solver s)))

(defn n-steps [s g] (dec (count (solve-path s g))))

(solve-path 7 43)
(n-steps 7 43)

(let [[a [{:keys [visited]} & r]](split-with
             (fn [{:keys [visited]}] (not (visited 43)))
             (iterate expand {:visited {} :queue (conj PersistentQueue/EMPTY [7 :done])}))]
  (reverse (take-while (complement #{:done}) (iterate visited 43))))

(some
  (fn [{:keys [visited]}] (when (visited 43) (reverse (take-while (complement #{:done}) (iterate visited 43)))))
  (iterate expand {:visited {} :queue (conj PersistentQueue/EMPTY [7 :done])}))

(defn iterative-number-maze [s g]
  (letfn [(nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (zero? (mod x 2)) (conj (/ x 2))))
          (expand [{v :visited [[n :as edge]] :queue :as m}]
            (cond-> (update m :queue pop) (not (v n))
                    (->
                      (update :visited conj edge)
                      (update :queue into (map (fn [x] [x n]) (nbrs n))))))
          (number-maze-solver [start]
            (iterate expand {:visited {} :queue (conj PersistentQueue/EMPTY [start :done])}))]
    (some
      (fn [{:keys [visited]}] (when (visited g) (reverse (take-while (complement #{:done}) (iterate visited g)))))
      (number-maze-solver s))))

(iterative-number-maze 7 92)

;(defn expand [{:keys [path visited]}]
;  (let [f (peek (peek path))
;        o (cond-> [[f '+ 2 '= (+ f 2)] [f '* 2 '= (* f 2)]]
;                  (zero? (mod f 2)) (conj [f '/ 2 '= (/ f 2)]))]
;    [f o]
;    #_(mapv
;      (fn [n]{:path (conj (pop path) n) :visited (conj visited o)})
;      o)))
;
;(expand {:path [:start 4] :visited #{}})
