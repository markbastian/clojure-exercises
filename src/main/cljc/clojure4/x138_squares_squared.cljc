(ns clojure4.x138-squares-squared)

(defn n->d [n]
  (loop [q (quot n 10) res (list (mod n 10))]
    (if (zero? q) res (recur (quot q 10) (conj res (mod q 10))))))

(defn seq-digs [s f]
  (->> s
       (iterate #(* % %))
       (take-while #(<= % f))
       (mapcat n->d)
       vec
       (iterate #(conj % '*))
       (filter #(zero? (rem (Math/sqrt (count %)) 1)))
       first))

(def dirs {[1 1] [1 -1]
           [1 -1] [-1 -1]
           [-1 -1] [-1 1]
           [-1 1] [1 1]})

(defn step [{:keys [grid start dir] :as m} v]
  (let [ndir (if (get-in grid (mapv + start (dirs dir))) dir (dirs dir))]
    (-> m
        (update :mn #(mapv min % start))
        (update :mx #(mapv max % start))
        (update :grid assoc-in start v)
        (assoc :dir ndir)
        (update :start #(mapv + % ndir)))))

(defn solve-grid [digs]
  (reduce step {:start [0 0] :grid {} :dir [-1 1] :mn [0 0] :mx [0 0]} digs))

(defn m->g [{g :grid [min-r min-c] :mn [max-r max-c] :mx}]
  (for [r (range min-r (inc max-r))]
    (apply str
           (for [c (range min-c (inc max-c))]
             (get-in g [r c] " ")))))

(defn sqsq [s f]
  (->> (seq-digs s f) solve-grid m->g))

;(print (sqsq 2 4))
;(prn (sqsq 2 256))
;(println (sqsq 3 81))
;(print (sqsq 10 10000))

(def __
  (let [dirs {[1 1] [1 -1] [1 -1] [-1 -1] [-1 -1] [-1 1] [-1 1] [1 1]}]
    (letfn
      [(n->d [n]
         (loop [q (quot n 10) res (list (mod n 10))]
           (if (zero? q) res (recur (quot q 10) (conj res (mod q 10))))))
       (seq-digs [s f]
         (->> s
              (iterate #(* % %))
              (take-while #(<= % f))
              (mapcat n->d)
              vec
              (iterate #(conj % '*))
              (filter #(zero? (rem (Math/sqrt (count %)) 1)))
              first))
       (step [{:keys [grid start dir] :as m} v]
         (let [ndir (if (get-in grid (mapv + start (dirs dir))) dir (dirs dir))]
           (-> m
               (update-in [:mn] #(mapv min % start))
               (update-in [:mx] #(mapv max % start))
               (update-in [:grid] assoc-in start v)
               (assoc-in [:dir] ndir)
               (update-in [:start] #(mapv + % ndir)))))
       (solve-grid [digs]
         (reduce step {:start [0 0] :grid {} :dir [-1 1] :mn [0 0] :mx [0 0]} digs))
       (m->g [{g :grid [min-r min-c] :mn [max-r max-c] :mx}]
         (for [r (range min-r (inc max-r))]
           (apply str
                  (for [c (range min-c (inc max-c))]
                    (get-in g [r c] " ")))))]
      (fn sqsq [s f]
        (->> (seq-digs s f) solve-grid m->g)))))

(assert
  (= (__ 2 2) ["2"]))

(assert
  (= (__ 2 4) [" 2 "
               "* 4"
               " * "]))

(assert
  (= (__ 3 81) [" 3 "
                "1 9"
                " 8 "]))

(assert
  (= (__ 4 20) [" 4 "
                "* 1"
                " 6 "]))

(assert
  (= (__ 2 256) ["  6  "
                 " 5 * "
                 "2 2 *"
                 " 6 4 "
                 "  1  "]))

(assert
  (= (__ 10 10000) ["   0   "
                    "  1 0  "
                    " 0 1 0 "
                    "* 0 0 0"
                    " * 1 * "
                    "  * *  "
                    "   *   "]))