(ns clojure-exercises.numbers)

(defn int->digits
  ([n base]
   (let [md (juxt #(mod % base) #(quot % base))]
     (loop [[m d] (md n) digits nil]
       (if (zero? d)
         (conj digits m)
         (recur (md d) (conj digits m))))))
  ([n] (int->digits n 10)))

(defn digits->str
  "Maps a sequence of digits to a string using alphadecimal characters (max base 36)."
  [digits]
  (let [v (mapv char
                (concat
                  (range (int \0) (inc (int \9)))
                  (range (int \a) (inc (int \z)))))]
    (apply str (map v digits))))

(comment
  (int->digits -19)
  (int->digits 0)
  (int->digits 43753311360000250 12)
  (-> (int->digits 43753311360000250 12) digits->str)
  (digits->str (int->digits (dec (* 36 36)) 36))
  )