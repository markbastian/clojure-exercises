(ns clojure4.unsolved.x084-transitive-closure)

;(def __)
;(assert (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
;          (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))
;(assert (let [more-legs
;              #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
;          (= (__ more-legs)
;             #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
;               ["spider" "cat"] ["spider" "man"] ["spider" "snake"]})))
;(assert (let [progeny
;              #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
;          (= (__ progeny)
;             #{["father" "son"] ["father" "grandson"]
;               ["uncle" "cousin"] ["son" "grandson"]})))