(ns clojure-exercises.sorting)

(defn q-sort [[f & r]]
  (let [{gt true lt false} (group-by #(neg? (compare f %)) r)
        l (if (seq lt) (q-sort lt) [])
        g (if (seq gt) (q-sort gt) [])]
    (into (conj l f) g)))

(comment
  (q-sort (into (shuffle (range 10))
                (shuffle (range 10)))))
