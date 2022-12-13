(ns clojure-exercises.coins
  (:require [clojure.pprint :as pp])
  (:import (clojure.lang PersistentQueue)))

;;;; What we’re looking for:

;;; Working code. You should be able to run this from the command line or repl and have it work.
;;;
;;; We don’t really care which language you use, but we recommend something high level.
;;;
;;; We’re interested in the general style of code, how you use your editor, naming conventions etc.
;;;
;;; There are many ways to solve this, but we suggest using a simple approach, as it is not primarily an algorithmic
;;; exercise (though bonus points for knowing and being able to implement the best approaches).
;;;
;;; Also, just so you know we’re serious — the code _must_ work. Pseudo code or code with syntactic errors is an
;;; automatic 0.

;;;; The problem

;;; Given a number `x` and a sorted array of coins `coinset`, write a function that finds a combination of these coins
;;; that add up to X There are an infinite number of each coin. This is hopefully familiar to making change for a
;;; given amount of money in a currency, but it gets more interesting if we remove the 1 coin and have “wrong” coins
;;; in the coinset.
;;;
;;; Return a map (or dictionary or whatever it is called in your preferred programming language such that each key is
;;; the coin, and each value is the number of times you need that coin. You need to only return a single solution, but
;;; for bonus points, return the one with the fewest number of coins. Don’t worry about performance or scalability for
;;; this problem.

;;;; A Specific example

;;; If x=5 and the coinset= [1,5,10,25], then the following are both solutions:
;;; `{1: 7}` since  7*1 = 7
;;; `{1: 2, 5: 1}` since 1*2 + 5*1=7

;;; # Some test cases for you to verify your approach works
;;; A. x = 6 coinset = [1,5,10,25]
;;; B. x = 6, coinset = [3,4]
;;; C. x = 6, coinset = [1,3,4]
;;; D. x = 6, coinset = [5,7]
;;; E. x = 16, coinset = [5,7,9]

(defn step
  "Expand the frontier of coins from the current set."
  [{:keys [coinset results] :as state}]
  (let [next-gen (for [[result] results
                       coin coinset
                       :let [r (update result coin (fnil inc 0))
                             v (reduce + (map * (keys r) (vals r)))]]
                   [r v])]
    (assoc state :results next-gen)))

(comment
  (let [goal 31]
    (->> {:goal goal :coinset [4 9] :results [[{} 0]]}
         (iterate step)
         ;; Stop iterating when every result in an iteration is greater than
         ;; the goal.
         (take-while (fn [{:keys [results]}]
                       (some (fn [[_ v]] (<= v goal)) results)))
         ;; Concatenate all of the results together
         (mapcat :results)
         ;; Find an actual solution. Will return nil if no result is found.
         (filter (fn [[_ v]] (= v goal)))
         ffirst)))

(defn make-change [goal coinset]
  (->> {:goal goal :coinset coinset :results [[{} 0]]}
       (iterate step)
       ;; Stop iterating when every result in an iteration is greater than
       ;; the goal.
       (take-while (fn [{:keys [results]}]
                     (some (fn [[_ v]] (<= v goal)) results)))
       ;; Concatenate all of the results together
       (mapcat :results)
       ;; Find an actual solution. Will return nil if no result is found.
       (filter (fn [[_ v]] (= v goal)))
       ffirst))

(comment
  (= {1 1 5 1} (make-change 6 [1 5 10 25]))
  (= {3 2} (make-change 6 [3 4]))
  (= {3 2} (make-change 6 [1 3 4]))
  (= nil (make-change 6 [5 7]))
  (= {7 1 9 1} (make-change 16 [5 7 9]))
  (= {25 1 5 1 1 1} (make-change 31 [1 5 10 25])))