(ns clojure4.x111-crossword-puzzle)

;http://www.4clojure.com/problem/111
;Crossword puzzle

(def __
  (letfn [(valid-row [goal row]
            (loop [[a b] (split-with (complement #{\#}) (remove #{\space} row))]
              (cond
                (and (= (count a) (count goal))
                     (every? true? (map (fn [a b] (or (= a b) (= b \_))) goal a))) true
                (empty? b) false
                :default (recur (split-with (complement #{\#}) (rest b))))))]
    (fn [word puzzle]
      (let [v (partial valid-row word)]
        (true? (or (some v puzzle)
                   (some v (apply map vector puzzle))))))))

(assert (= true  (__ "the" ["_ # _ _ e"])))

(assert (= false (__ "the" ["c _ _ _"
                            "d _ # e"
                            "r y _ _"])))

(assert (= true  (__ "joy" ["c _ _ _"
                            "d _ # e"
                            "r y _ _"])))

(assert (= false (__ "joy" ["c o n j"
                            "_ _ y _"
                            "r _ _ #"])))

(assert (= true  (__ "clojure" ["_ _ _ # j o y"
                                "_ _ o _ _ _ _"
                                "_ _ f _ # _ _"])))