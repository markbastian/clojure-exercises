(ns clojure-exercises.levenshtein)

(defn lev [[af & ar :as a] [bf & br :as b]]
  (cond
    (nil? bf) (count a)
    (nil? af) (count b)
    (= af bf) (lev ar br)
    :else (inc (min (lev ar b) (lev a br) (lev ar br)))))

(comment
  (= 1 (lev "kitten" "sitten"))
  (= 3 (lev "kitten" "sitting")))
