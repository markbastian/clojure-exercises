(ns clojure4.unsolved.x138-squares-squared
  (:require [clojure.pprint :as pp]))

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

(defn grid [s]
  (let [n (int (Math/sqrt (count s)))
        r (vec (interpose \space (repeat n \space)))]
    {:start [(dec n) (dec n)]
     :grid (vec (repeat (count r) r))
     :dir [-1 1]}))

(def dirs {[1 1] [1 -1]
           [1 -1] [-1 -1]
           [-1 -1] [-1 1]
           [-1 1] [1 1]})

(defn step [{:keys [grid start dir] :as m} v]
  (let [ndir (if (= \space (get-in grid (mapv + start (dirs dir)))) (dirs dir) dir)]
    (-> m
        (update :grid assoc-in start v)
        (assoc :dir ndir)
        (update :start #(mapv + % ndir)))))

(defn sqsq [s f]
  (->> (let [s (seq-digs s f)]
         (reduce
           step
           (grid s) s))
       :grid
       (map #(clojure.string/join %))
       (clojure.string/join "\n")))

(sqsq 2 256)

(def __
  (letfn []
    (fn [s f]
      (->> (let [s (seq-digs s f)]
             (reduce
               step
               (grid s) s))
           :grid
           (map #(clojure.string/join %))
           (clojure.string/join "\n")))))

(= (__ 2 2) ["2"])

(= (__ 2 4) [" 2 "
             "* 4"
             " * "])

(= (__ 3 81) [" 3 "
              "1 9"
              " 8 "])

(= (__ 4 20) [" 4 "
              "* 1"
              " 6 "])

(= (__ 2 256) ["  6  "
               " 5 * "
               "2 2 *"
               " 6 4 "
               "  1  "])

(= (__ 10 10000) ["   0   "
                  "  1 0  "
                  " 0 1 0 "
                  "* 0 0 0"
                  " * 1 * "
                  "  * *  "
                  "   *   "])