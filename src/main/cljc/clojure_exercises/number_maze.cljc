(ns clojure-exercises.number-maze
  (:require [clojure.string :as cs])
  (:import #?(:clj (clojure.lang PersistentQueue))))

(def empty-queue #?(:clj PersistentQueue/EMPTY :cljs #queue []))

;Produce only the number of steps.
(defn number-maze [s g]
  (letfn [(nbrs [x] (cond-> [(* 2 x) (+ x 2)] (even? x) (conj (/ x 2))))]
    (inc (count (take-while #(not (% g)) (iterate #(reduce into #{} (map nbrs %)) #{s}))))))

;(time (number-maze 1 4137))
;(time (number-maze 7 43))
;(time (number-maze 9 2))
;(time (number-maze 2 9))
;(time (number-maze 2 4))
;(time (number-maze 2 2))

;Return a single path
(defn number-path [s g]
  (letfn [(nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (even? x) (conj (/ x 2))))]
    (loop [v {} [[n :as edge] :as q] (conj empty-queue [s :done])]
      (cond
        (v g) (reverse (take-while (complement #{:done}) (iterate v g)))
        (v n) (recur v (pop q))
        :default (recur (conj v edge) (into (pop q) (map (fn [x] [x n]) (nbrs n))))))))

;(time (number-path 1 4137))
;(time (number-path 7 43))
;(time (number-path 9 2))
;(time (number-path 2 9))
;(time (number-path 2 4))
;(time (number-path 2 2))

;Return them all - very slow
(defn number-paths [s g]
  (letfn [(nbrs [x] (cond-> [['* (* 2 x)] ['+ (+ x 2)]] (even? x) (conj ['/ (/ x 2)])))
          (expand [paths] (for [path paths p (nbrs (peek path))
                                :when (not-any? #{(second p)} path)] (into path p)))]
    (->> (iterate expand [[s]])
         (some #(when (some (comp #{g} peek) %) %))
         (filter (comp #{g} peek)))))

;(time (number-paths 1 4137))
;(time (number-paths 7 43))
;(time (number-paths 9 2))
;(time (number-paths 2 9))
;(time (number-paths 2 4))
;(time (number-paths 2 2))

;Return them all
(defn fast-number-paths [s g]
  (letfn [(nbrs [x] (cond-> [['* (* 2 x)] ['+ (+ x 2)]] (even? x) (conj ['/ (/ x 2)])))
          (expand [{:keys [paths visited]}]
            (let [p (for [path paths n (remove (comp visited second) (nbrs (peek path)))] (into path n))]
              {:paths p :visited (into visited (map peek p))}))]
    (let [pred (comp #{g} peek)]
      (->> (iterate expand {:paths [[s]] :visited #{s}})
           (some (fn [{:keys [paths]}] (when (some pred paths) paths)))
           (filter pred)))))

;(time (fast-number-paths 1 4137))
;(time (fast-number-paths 7 43))
;(time (fast-number-paths 9 2))
;(time (fast-number-paths 2 9))
;(time (fast-number-paths 2 4))
;(time (fast-number-paths 2 2))

;The below code deconstructs the above function to demonstrate how easy it is to
; assemble/dissassemble and debug Clojure forms.

;How the front grows
(defn nbrs [x] (cond-> [['* (* 2 x)] ['+ (+ x 2)]] (even? x) (conj ['/ (/ x 2)])))

;One generation/step of growth - Must be self-similar for iteration
(defn expand-step [{:keys [paths visited]}]
  (let [p (for [path paths n (remove (comp visited second) (nbrs (peek path)))] (into path n))]
    {:paths p :visited (into visited (map peek p))}))

;Lazy sequence that expands all paths.
; Note that iterate vs. loop-recur separates concerns of iteration from termination.
(defn expand-from [s]
  (iterate expand-step {:paths [[s]] :visited #{s}}))

;When to stop
(defn stop-cond [pred {:keys [paths]}] (when (some pred paths) paths))

;The entire solution
(defn fast-number-paths-deconstructed [s g]
  (let [pred (comp #{g} peek)]
    (->> (expand-from s)
         (some (partial stop-cond pred))
         (filter pred))))

;(time (fast-number-paths-deconstructed 1 4137))
;(time (fast-number-paths-deconstructed 7 43))
;(time (fast-number-paths-deconstructed 9 2))
;(time (fast-number-paths-deconstructed 2 9))
;(time (fast-number-paths-deconstructed 2 4))
;(time (fast-number-paths-deconstructed 2 2))

;;;;;;;;;;;;;;;;;;Generalized Solver;;;;;;;;;;;;;;;;;;;;;;;
(defn expand [{v :visited [[n :as edge]] :queue nbrs :neighbors :as m}]
  (cond-> (update m :queue pop) (not (v n))
          (->
            (update :visited conj edge)
            (update :queue into (map (fn [x] [x n]) (nbrs n))))))

(defn bfs-expander [{:keys [start neighbors]}]
  (iterate expand {:visited   {}
                   :queue     (conj empty-queue [start :done])
                   :neighbors neighbors}))

(defn rsolve-path [goal {:keys [visited]}]
  (when (visited goal) (reverse (take-while (complement #{:done}) (iterate visited goal)))))

(defn bfs-solve [& {:keys [goal] :as ic}]
  (some (fn [{:keys [queue] :as m}]
          (or (rsolve-path goal m) (when (empty? queue) :no-solution)))
        (bfs-expander ic)))

;Solve today's problem
(bfs-solve
  :start 7
  :goal 43
  :neighbors (fn nbrs [x] (cond-> [(+ 2 x) (* 2 x)] (even? x) (conj (/ x 2)))))

;Use another function to generate neighbors
(bfs-solve
  :start 1
  :goal 7
  :neighbors {1 [2] 2 [3 5] 3 [4]})

;Get creative....
(def dungeon
  ["XXXXXXXX XXXXXXXX"
   "X     XX XX    TX"
   "X XXX       XXXXX"
   "X XXXX XX XXXXX X"
   "X  XXX XXXX     X"
   "XX  XXXX    XXX X"
   "XXX      XXXXXX X"
   "XXS   XXXXXXXXX X"])

(defn find-cell [d sym]
  (first (for [i (range (count d)) j (range (count (d i)))
               :when (= sym (get-in d [i j]))] [i j])))

(defn square-neighbors [[i j]]
  (map vector
       ((juxt inc identity dec identity) i)
       ((juxt identity inc identity dec) j)))

(defn dungeon-neigbors [d c]
  (filter #(#{\space \S \T} (get-in d %)) (square-neighbors c)))

(def dungeon-path
  (bfs-solve
    :start (find-cell dungeon \S)
    :goal (find-cell dungeon \T)
    :neighbors (partial dungeon-neigbors dungeon)))

(comment
  (if (not= :no-solution dungeon-path)
    (map cs/join
         (reduce #(assoc-in %1 %2 \.)
                 (mapv vec dungeon)
                 (rest (butlast dungeon-path))))
    "No solution!")
  )