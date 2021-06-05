(ns clojure-exercises.simple
  (:require [clojure.edn :as edn]))

(defn clock-angle [t]
  (let [[_ h m] (re-matches #"(\d+):(\d+)" t)
        h (edn/read-string h)
        m (edn/read-string m)
        minute-angle (* 6.0 m)
        hour-angle (+ (* 30.0 (mod h 12)) (/ minute-angle 12.0))]
    {:hour-angle hour-angle
     :min-angle  minute-angle
     :angle      (Math/abs (- hour-angle minute-angle))}))

(comment
  (clock-angle "12:45")
  (clock-angle "7:01")
  )

