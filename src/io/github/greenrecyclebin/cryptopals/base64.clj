(ns io.github.greenrecyclebin.cryptopals.base64
  (:import
   (java.nio ByteBuffer)
   (java.nio.charset Charset)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- char-range [start end]
  (map char (range (int start) (int end))))

(def ^:private base64
  (into []
        (concat (char-range \A (inc (int \Z)))
                (char-range \a (inc (int \z)))
                (char-range \0 (inc (int \9)))
                [\+ \/])))

(declare hex-string->ByteBuffer
         ByteBuffer->base64-ByteBuffer
         base64-ByteBuffer->base64-string)

(defn s->base64
  ([s]
   (s->base64 s (Charset/defaultCharset)))

  ([^String s ^Charset charset]
   (-> s
       (.getBytes charset)
       ByteBuffer/wrap
       ByteBuffer->base64-ByteBuffer
       base64-ByteBuffer->base64-string)))

(defn hex-string->base64-string [s]
  (-> s
      hex-string->ByteBuffer
      ByteBuffer->base64-ByteBuffer
      base64-ByteBuffer->base64-string))

(defn- hex-string->ByteBuffer [^String s]
  (let [size (count s)
        capacity (quot size 2)
        bs (ByteBuffer/allocate capacity)]
    (doseq [^int i (range 0 size 2)]
      (.put bs (-> s
                   (subs i (+ i 2))
                   (Short/parseShort 16)
                   unchecked-byte)))

    (.position bs 0)))

(defn- ByteBuffer->base64-ByteBuffer [^ByteBuffer bs]
  (let [size (.remaining bs)
        group-of-3-byte-count (quot size 3)
        group-of-3-byte-end (* group-of-3-byte-count 3)
        byte-in-last-group-count (- size group-of-3-byte-end)

        obs (ByteBuffer/allocate (* 4 (int (Math/ceil (/ size 3)))))]
    (doseq [_ (range 0 group-of-3-byte-end 3)
            :let [bits (bit-or (-> (.get bs)
                                   (bit-and 0xFF)
                                   (bit-shift-left 16))

                               (-> (.get bs)
                                   (bit-and 0xFF)
                                   (bit-shift-left 8))

                               (-> (.get bs)
                                   (bit-and 0xFF)))]]
      (doseq [^int i (range 18 (dec 0) -6)]
        (.put obs (-> bits
                      (unsigned-bit-shift-right i)
                      (bit-and 0x3F)
                      base64
                      int
                      byte))))

    (case byte-in-last-group-count
      0 obs

      (do
        (let [b0 (-> (.get bs)
                     (bit-and 0xFF))]
          (.put obs (-> b0
                        (bit-shift-right 2)
                        base64
                        int
                        byte))

          (case byte-in-last-group-count
            1 (do
                (.put obs (-> b0
                              (bit-shift-left 4)
                              (bit-and 0x3F)
                              base64
                              int
                              byte))
                (.put obs (-> \=
                              int
                              byte))
                (.put obs (-> \=
                              int
                              byte)))

            2 (let [b1 (-> (.get bs)
                           (bit-and 0xFF))]
                (.put obs (-> b0
                              (bit-shift-left 4)
                              (bit-and 0x3F)
                              (bit-or (bit-shift-right b1 4))
                              base64
                              int
                              byte))

                (.put obs (-> b1
                              (bit-shift-left 2)
                              (bit-and 0x3F)
                              base64
                              int
                              byte))

                (.put obs (-> \=
                              int
                              byte)))))))

    (.position obs 0)))

(defn- base64-ByteBuffer->base64-string [^ByteBuffer bs]
  (loop [cs []]
    (if (zero? (.remaining bs))
      (apply str cs)
      (recur (conj cs (-> bs
                          .get
                          char))))))
