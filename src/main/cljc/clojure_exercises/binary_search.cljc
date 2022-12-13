(ns clojure-exercises.binary-search)

(defn binary-search
  ([v lo hi goal]
   (let [mid (quot (+ lo hi) 2)
         vmid (v mid)]
     (println [lo mid hi])
     (cond
       (= goal vmid) {:index mid}
       (> lo hi) {:insert-at lo}
       (> goal vmid) (binary-search v (inc mid) hi goal)
       (< goal vmid) (binary-search v lo (dec mid) goal))))
  ([v goal]
   (cond
     (or (empty? v) (< goal (v 0))) {:insert-at 0}
     (> goal (peek v)) {:insert-at (count v)}
     :else (binary-search v 0 (dec (count v)) goal))))

(comment
  (let [v [0 2 5 23 45 67 90 100]]
    [(binary-search [] 1)
     (binary-search v -1)
     (binary-search v 5)
     (binary-search v 67)
     (binary-search v 100)
     (binary-search v 1000)]))
