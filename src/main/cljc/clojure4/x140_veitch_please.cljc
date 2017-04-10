(ns clojure4.x140-veitch-please)

(defn complementary-symbol[sym]
  (let [s (str sym)
        f (if (re-matches #"[A-Z]" s)
            clojure.string/lower-case
            clojure.string/upper-case)]
    (symbol (f s))))

(defn complementary-symbols [soln]
  (mapv (fn [s]#{s (complementary-symbol s)}) soln))

(complementary-symbols #{'a 'B 'C 'd})

(def __ (fn [s]#{}))

(def baf
  #{#{'a 'B 'C 'd}
    #{'A 'b 'c 'd}
    #{'A 'b 'c 'D}
    #{'A 'b 'C 'd}
    #{'A 'b 'C 'D}
    #{'A 'B 'c 'd}
    #{'A 'B 'c 'D}
    #{'A 'B 'C 'd}})

(let [[f & r] baf]
  (map #(clojure.set/intersection f %) r))

(let [syms (complementary-symbols (first baf))
      v (vec baf)]
  (for [i (range (count v)) j (range (inc i) (count v)) k (range (count syms))
        :let [a (clojure.set/difference (v i) (syms k))
              b (clojure.set/difference (v j) (syms k))]
        :when (= a b)]
    a))

(defn frak [baf opts]
  (let [simplified (->> baf
                        (map #(clojure.set/difference % opts))
                        frequencies
                        (filter (comp #{2} second))
                        (map first))
        removals (into #{} (for [s simplified o opts] (conj s o)))]
    (into (clojure.set/difference baf removals) simplified)))

(frak (frak (frak (frak baf #{'D 'd}) #{'C 'c}) #{'B 'b}) #{'A 'a})

(frak (frak baf #{'B 'b}) #{'D 'd})

(reduce
  frak
  baf
  (complementary-symbols (first baf)))

(= (__ #{#{'a 'B 'C 'd}
         #{'A 'b 'c 'd}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}
         #{'A 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'c}
     #{'A 'b}
     #{'B 'C 'd}})

(= (__ #{#{'A 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'B 'C}})

(= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'C 'd}
         #{'A 'B 'C 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}})
   #{#{'a 'c}
     #{'A 'C}})

(= (__ #{#{'a 'b 'c}
         #{'a 'B 'c}
         #{'a 'b 'C}
         #{'a 'B 'C}})
   #{#{'a}})

(= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'b 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'a 'B 'c 'd}
     #{'A 'B 'c 'D}
     #{'A 'b 'C 'D}
     #{'a 'b 'c 'D}
     #{'a 'B 'C 'D}
     #{'A 'B 'C 'd}})

(= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}})
   #{#{'a 'c}
     #{'B 'c}})

(= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'b 'C 'D}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'B 'C 'd}
         #{'A 'B 'C 'd}})
   #{#{'B 'd}
     #{'b 'D}})

(= (__ #{#{'a 'b 'c 'd}
         #{'A 'b 'c 'd}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'D}
         #{'a 'b 'C 'd}
         #{'A 'b 'C 'd}})
   #{#{'B 'D}
     #{'b 'd}})