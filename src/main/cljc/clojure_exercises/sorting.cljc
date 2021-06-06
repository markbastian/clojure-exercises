(ns clojure-exercises.sorting)

(defn bubble
  "Naive n^2 sorting algorithm. Stable."
  [xs]
  (loop [[f n & r] xs res [] sorted? true]
    (if (and f n)
      (if (<= f n)
        (recur (cons n r) (conj res f) sorted?)
        (recur (cons f r) (conj res n) false))
      (let [res (cond-> res f (conj f))]
        (if sorted?
          res
          (recur res [] true))))))

(comment
  (apply < (bubble (shuffle (range 100))))
  (bubble (shuffle (range 10)))
  (bubble (into
            (shuffle (range 5))
            (shuffle (range 5))))
  (bubble nil)
  (bubble [1])
  (bubble [1 2])
  (bubble [2 1])
  )

(defn q-sort
  "Attributes O(n*logn), unstable."
  [[f & r]]
  (when f
    (let [{gt true lt false} (group-by #(neg? (compare f %)) r)
          l (if (seq lt) (q-sort lt) [])
          g (if (seq gt) (q-sort gt) [])]
      (into (conj l f) g))))

(comment
  (apply < (q-sort (shuffle (range 100))))
  (q-sort (shuffle (range 10)))
  (q-sort (into
            (shuffle (range 5))
            (shuffle (range 5))))
  (q-sort nil)
  (q-sort [1])
  (q-sort [1 1 1 1 1])
  (q-sort [1 2])
  (q-sort [2 1])
  )

(defn merge-sorted
  "Merge two sorted seqs into a single sorted seq"
  ([a b]
   (loop [[fa & ra :as a] a [fb & rb :as b] b res []]
     (cond
       (and fa fb) (if (< fa fb)
                     (recur ra b (conj res fa))
                     (recur a rb (conj res fb)))
       fa (recur ra b (conj res fa))
       fb (recur a rb (conj res fb))
       :else res)))
  ([a] a))

(comment
  (merge-sorted [1 2 3 4] [3 4 5 6])
  (merge-sorted [1 2 3 4]))

(defn merge-sort
  "Attributes O(n*logn), stable."
  [xs]
  (loop [[l1 l2 :as lists] (map list xs)]
    (if l2
      (recur (map #(apply merge-sorted %) (partition-all 2 lists)))
      l1)))

(comment
  (apply < (merge-sort (shuffle (range 100))))
  (merge-sort (shuffle (range 10)))
  (merge-sort (into
                (shuffle (range 5))
                (shuffle (range 5))))
  (merge-sort nil)
  (merge-sort [1])
  (merge-sort [1 1 1 1 1])
  (merge-sort [1 2])
  (merge-sort [1 2 3])
  (merge-sort [2 1]))