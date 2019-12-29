(ns io.github.greenrecyclebin.cryptopals.xor-test
  (:require [io.github.greenrecyclebin.cryptopals.xor :refer [hex-string-xor]
             :rename {hex-string-xor xor}]

            [clojure.test :refer :all]))

(deftest hex-string-xor
  (is (= "746865206b696420646f6e277420706c6179"

         (xor "1c0111001f010100061a024b53535009181c"
              "686974207468652062756c6c277320657965"))))
