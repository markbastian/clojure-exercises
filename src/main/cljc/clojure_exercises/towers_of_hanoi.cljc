(ns clojure-exercises.towers-of-hanoi)

(defn neighbors [[l c r]]
  (let [f (juxt
            (fn [v] (peek v))
            (fn [v] (if (empty? v) v (pop v))))
        [tl rl] (f l)
        [tc rc] (f c)
        [tr rr] (f r)]
    (cond->
      []
      (and tl (or (empty? c) (< tl tc)))
      (conj [rl (conj c tl) r])
      (and tl (or (empty? r) (< tl tr)))
      (conj [rl c (conj r tl)])
      (and tc (or (empty? l) (< tc tl)))
      (conj [(conj l tc) rc r])
      (and tc (or (empty? r) (< tc tr)))
      (conj [l rc (conj r tc)])
      (and tr (or (empty? l) (< tr tl)))
      (conj [(conj l tr) c rr])
      (and tr (or (empty? c) (< tr tc)))
      (conj [l (conj c tr) rr]))))

(defn step [{:keys [frontier visited goal] :as state}]
  (let [nxt (peek frontier)]
    (if (not= nxt goal)
      (let [nbrs (remove #(contains? visited %) (neighbors nxt))]
        (-> state
            (update :frontier (fn [q] (into (pop q) nbrs)))
            (update :visited (fn [m] (into m (for [n nbrs] [n nxt])))))))))

(defn steps [m goal]
  (reverse (take-while identity (iterate m goal))))

(defn solve-towers [towers]
  (let [goal (reverse towers)
        {:keys [visited]}
        (->> {:frontier (conj clojure.lang.PersistentQueue/EMPTY towers)
              :visited  {towers nil}
              :goal     goal}
             (iterate step)
             (take-while identity)
             last)]
    (steps visited goal)))

(comment
  (solve-towers
    [[5 4 3 2 1]
     []
     []]))

