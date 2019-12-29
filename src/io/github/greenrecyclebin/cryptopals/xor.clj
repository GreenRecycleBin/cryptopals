(ns io.github.greenrecyclebin.cryptopals.xor
  (:require
   [io.github.greenrecyclebin.cryptopals.utils :refer
    [hex-string->ByteBuffer ByteBuffer->hex-string]])

  (:import
   (java.nio ByteBuffer)))

(defn hex-string-xor [s1 s2]
  {:pre [(= (count s1) (count s2))]}

  (let [bs1 (hex-string->ByteBuffer s1)
        bs2 (hex-string->ByteBuffer s2)
        capacity (.remaining bs1)
        obs (ByteBuffer/allocate capacity)]
    (dotimes [_ capacity]
      (let [b1 (.get bs1)
            b2 (.get bs2)]
        (.put obs (-> (bit-xor b1 b2)
                      unchecked-byte))))

    (-> obs
        (.position 0)
        ByteBuffer->hex-string)))
