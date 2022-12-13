(ns clojure-exercises.towers-of-hanoi)

(defn move-to [s fi ti]
  (let [a (s fi)
        ta (peek a)
        b (s ti)
        tb (peek b)]
    (when (and ta (or (empty? b) (< ta tb)))
      (-> s
          (update fi pop)
          (update ti conj ta)))))

(defn neighbors [ic]
  (letfn [(add-neighbor [acc [fi ti]]
            (if-some [n (move-to ic fi ti)] (conj acc n) acc))]
    (reduce add-neighbor [] [[0 1] [0 2] [1 0] [1 2] [2 0] [2 1]])))

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
    [[3 2 1]
     []
     []]))
