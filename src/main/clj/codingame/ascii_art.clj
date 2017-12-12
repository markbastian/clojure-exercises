(ns codingame.ascii-art
  (:require [clojure.java.io :as io]))

;https://www.codingame.com/ide/puzzle/ascii-art
(-> "ascii_art_example1.txt"
    io/resource
    slurp)
