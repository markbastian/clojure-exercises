(ns clojure4.x140-veitch-please)

(defn complementary-symbol[sym]
  (let [s (str sym)
        f (if (re-matches #"[A-Z]" s)
            clojure.string/lower-case
            clojure.string/upper-case)]
    (symbol (f s))))

(defn complementary-symbols [soln]
  (mapv (fn [s]#{s (complementary-symbol s)}) soln))


(defn expand [baf minterms syms]
  (let [expansion (set
                    (for [minterm minterms sym syms
                          :let [x (conj (clojure.set/difference minterm syms) sym)]
                          :when (baf x)]
                      x))]
    (if (= (* 2 (count minterms)) (count expansion))
      expansion
      minterms)))

(defn group-minterms [baf minterm]
  (reduce
    (partial expand baf)
    #{minterm}
    (complementary-symbols minterm)))

(defn simplify [equivalent-minterms]
  (reduce clojure.set/intersection equivalent-minterms))

(defn boolean-reduce[baf]
  (loop [minterms baf equivalences []]
    (if (not-empty minterms)
      (let [e (group-minterms baf (first minterms))]
        (recur (clojure.set/difference minterms e) (conj equivalences e)))
      (set
        (for [r equivalences
            :let [c (reduce into #{} (remove #{r} equivalences))]
            :when (not-empty (clojure.set/difference r c))]
        (simplify r))))))

(def __
  (fn [baf]
    (loop [minterms baf equivalences []]
      (if (not-empty minterms)
        (let [e (reduce
                  (fn [minterms syms]
                    (let [expansion (->> minterms
                                         (mapcat (fn [p] (map #(conj (clojure.set/difference p syms) %) syms)))
                                         (filter baf)
                                         set)]
                      (if (= (* 2 (count minterms)) (count expansion))
                        expansion
                        minterms)))
                  #{(first minterms)} [#{'A 'a} #{'B 'b} #{'C 'c} #{'D 'd}])]
          (recur (clojure.set/difference minterms e) (conj equivalences e)))
        (set
          (for [r equivalences
                :let [c (reduce into #{} (remove #{r} equivalences))]
                :when (not-empty (clojure.set/difference r c))]
            (reduce clojure.set/intersection r)))))))

(assert
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
       #{'B 'C 'd}}))

(assert
  (= (__ #{#{'A 'B 'C 'D}
           #{'A 'B 'C 'd}})
     #{#{'A 'B 'C}}))

(assert
  (= (__ #{#{'a 'b 'c 'd}
           #{'a 'B 'c 'd}
           #{'a 'b 'c 'D}
           #{'a 'B 'c 'D}
           #{'A 'B 'C 'd}
           #{'A 'B 'C 'D}
           #{'A 'b 'C 'd}
           #{'A 'b 'C 'D}})
     #{#{'a 'c}
       #{'A 'C}}))

(assert
  (= (__ #{#{'a 'b 'c}
           #{'a 'B 'c}
           #{'a 'b 'C}
           #{'a 'B 'C}})
     #{#{'a}}))

(assert
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
       #{'A 'B 'C 'd}}))

(assert
  (= (__ #{#{'a 'b 'c 'd}
           #{'a 'B 'c 'd}
           #{'A 'B 'c 'd}
           #{'a 'b 'c 'D}
           #{'a 'B 'c 'D}
           #{'A 'B 'c 'D}})
     #{#{'a 'c}
       #{'B 'c}}))

(assert
  (= (__ #{#{'a 'B 'c 'd}
           #{'A 'B 'c 'd}
           #{'a 'b 'c 'D}
           #{'a 'b 'C 'D}
           #{'A 'b 'c 'D}
           #{'A 'b 'C 'D}
           #{'a 'B 'C 'd}
           #{'A 'B 'C 'd}})
     #{#{'B 'd}
       #{'b 'D}}))

(assert
  (= (__ #{#{'a 'b 'c 'd}
           #{'A 'b 'c 'd}
           #{'a 'B 'c 'D}
           #{'A 'B 'c 'D}
           #{'a 'B 'C 'D}
           #{'A 'B 'C 'D}
           #{'a 'b 'C 'd}
           #{'A 'b 'C 'd}})
     #{#{'B 'D}
       #{'b 'd}}))