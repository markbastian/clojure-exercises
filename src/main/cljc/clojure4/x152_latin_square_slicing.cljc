(ns clojure4.x152-latin-square-slicing)

(defn latin-square? [v]
  (let [r (into (map frequencies v) (map frequencies (apply map vector v)))]
    (when (apply = r) (-> r first keys set not-empty))))

(defn expand
  ([paths [n & r]]
   (if n (recur (for [path paths a n] (conj path a)) r) paths))
  ([options] (expand [[]] options)))

(defn pad [t f]
  (let [m (count t) l (count f) v (vec (repeat m nil))]
    (map
      (fn [s] (reduce #(assoc %1 (+ %2 s) (f %2)) v (range l)))
      (range (inc (- m l))))))

(defn pad-all [v]
  (let [l (apply max-key count v)]
    (map (partial pad l) v)))

(defn slice-square [s n]
  (->> s
       (map #(partition n 1 %))
       (apply map vector)
       (mapcat #(partition n 1 %))))

(defn square-count [s n]
  [n (->> s
          (mapcat #(slice-square % n))
          distinct
          (filter latin-square?)
          (filter (comp (partial every? identity) flatten))
          count)])

(defn latin-square-slicing [v]
  (let [[[c :as r] :as slices] (->> v pad-all expand)]
    (->> (range 1 (min (count c) (count r)))
         (map (comp (partial square-count slices) inc))
         (filter (comp pos? second))
         (into {}))))

(def __
  (fn [v]
    (letfn [(latin-square? [v]
              (let [r (into (map frequencies v) (map frequencies (apply map vector v)))]
                (when (apply = r) (-> r first keys set not-empty))))
            (expand
              ([paths [n & r]]
                (if n (recur (for [path paths a n] (conj path a)) r) paths))
              ([options] (expand [[]] options)))
            (pad [t f]
              (let [m (count t) l (count f) v (vec (repeat m nil))]
                (map
                  (fn [s] (reduce #(assoc %1 (+ %2 s) (f %2)) v (range l)))
                  (range (inc (- m l))))))
            (pad-all [v]
              (let [l (apply max-key count v)]
                (map (partial pad l) v)))
            (slice-square [s n]
              (->> s
                   (map #(partition n 1 %))
                   (apply map vector)
                   (mapcat #(partition n 1 %))))
            (square-count [s n]
              [n (->> s
                      (mapcat #(slice-square % n))
                      distinct
                      (filter latin-square?)
                      (filter (comp (partial every? identity) flatten))
                      count)])
            (latin-square-slicing [v]
              (let [[[c :as r] :as slices] (->> v pad-all expand)]
                (->> (range 1 (min (count c) (count r)))
                     (map (comp (partial square-count slices) inc))
                     (filter (comp pos? second))
                     (into {}))))]
      (latin-square-slicing v))))

(assert
  (= (__ '[[A B C D]
           [A C D B]
           [B A D C]
           [D C A B]])
     {}))

(assert
  (= (__ '[[A B C D E F]
           [B C D E F A]
           [C D E F A B]
           [D E F A B C]
           [E F A B C D]
           [F A B C D E]])
     {6 1}))

(assert
  (= (__ '[[A B C D]
           [B A D C]
           [D C B A]
           [C D A B]])
     {4 1, 2 4}))

(assert
  (= (__ '[[B D A C B]
           [D A B C A]
           [A B C A B]
           [B C A B C]
           [A D B C A]])
     {3 3}))

(assert
  (= (__ [[2 4 6 3]
          [3 4 6 2]
          [6 2 4]  ])
     {})
  )

(assert
  (= (__ [[1]
          [1 2 1 2]
          [2 1 2 1]
          [1 2 1 2]
          []       ])
     {2 2}))

(assert
  (= (__ [[3 1 2]
          [1 2 3 1 3 4]
          [2 3 1 3]    ])
     {3 1, 2 2}))

(assert
  (= (__ [[8 6 7 3 2 5 1 4]
          [6 8 3 7]
          [7 3 8 6]
          [3 7 6 8 1 4 5 2]
          [1 8 5 2 4]
          [8 1 2 4 5]])
     {4 1, 3 1, 2 7}))