(ns clojure-exercises.crypto-square
  (:require [clojure.string :as cs]))

(def a "If man was meant to stay on the ground god would have given us roots")
(def s "A beginning is the time for taking the most delicate care that the balances are correct.")

(defn crypto-square [s]
  (let [n (-> s (cs/replace #"\W+" "") cs/lower-case)
        l (-> n count Math/sqrt Math/ceil int)]
    (->> n
         (partition l l (repeat nil))
         (apply map (comp cs/join vector))
         (cs/join " "))))

(comment
  (prn (crypto-square s))
  (prn (crypto-square "If man was meant to stay on the ground god would have given us roots")))
