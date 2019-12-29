(ns io.github.greenrecyclebin.cryptopals.base64
  (:import
   (java.nio ByteBuffer)
   (java.nio.charset Charset))

  (:require
   [io.github.greenrecyclebin.cryptopals.utils :refer
    [hex-string->ByteBuffer
     ByteBuffer->base64-ByteBuffer
     base64-ByteBuffer->base64-string]]))

(set! *warn-on-reflection* true)

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
