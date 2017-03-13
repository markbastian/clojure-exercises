(ns clojure4.unsolved.x127-love-triange)

(defn n-bits [n m] (mapv #(if (bit-test m %) 1 0) (range n)))

(defn decode [s]
  (mapv (partial n-bits (int (Math/ceil (/ (Math/log (apply max s)) (Math/log 2))))) s))

(defn grow-a [[i j]] [[(inc i) j] [(inc i) (inc j)]])

(defn grow-b [[i j]] [[(inc i) (inc j)][(inc i) j][(inc i) (dec j)]])

(decode [15 15 15 15 15])
(decode [1 3 7 15 31])
(decode [3 3])
(decode [7 3])
(decode [17 22 6 14 22])
(decode [18 7 14 14 6 3])
(decode [21 10 21 10])
(decode [0 31 0 31 0])

;(= 10 (__ [15 15 15 15 15]))
;(= 15 (__ [1 3 7 15 31]))
;(= 3 (__ [3 3]))
;(= 4 (__ [7 3]))
;(= 6 (__ [17 22 6 14 22]))
;(= 9 (__ [18 7 14 14 6 3]))
;(= nil (__ [21 10 21 10]))
;(= nil (__ [0 31 0 31 0]))