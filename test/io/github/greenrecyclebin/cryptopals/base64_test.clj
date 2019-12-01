(ns io.github.greenrecyclebin.cryptopals.base64-test
  (:require
   [clojure.java.io :refer [reader]]
   [clojure.test :refer :all]
   [io.github.greenrecyclebin.cryptopals.base64 :refer :all]

   [io.github.greenrecyclebin.cryptopals.golden-file-utils :refer
    [create-encoded-with-basic-golden-file-if-needed
     encoded-with-basic-filename
     plaintext-filename]]

   [io.github.greenrecyclebin.cryptopals.test-utils :refer
    [root-directory path-to-resource]])

  (:import
   (java.nio.charset StandardCharsets)))

(def ^:private root-directory-of-ns (root-directory (ns-name *ns*)))

(defmacro base64-basic-encoding [charset]
  `(let [charset-name# (.name ~charset)]
     (testing (str "Base64 basic encoding - " charset-name#)
       (create-encoded-with-basic-golden-file-if-needed charset-name#)

       (with-open [plaintext-reader# (-> charset-name#
                                         plaintext-filename
                                         (path-to-resource root-directory-of-ns)
                                         reader)

                   encoded-with-basic-reader#
                   (-> charset-name#
                       encoded-with-basic-filename
                       (path-to-resource root-directory-of-ns) reader)]
         (let [lines# (line-seq plaintext-reader#)
               expected-lines# (line-seq encoded-with-basic-reader#)]
           (doseq [[line# expected#]
                   (partition 2 (interleave lines# expected-lines#))

                   :let [actual# (s->base64 line# ~charset)]]
             (is (= expected# actual#))))))))

(deftest encoding
  (base64-basic-encoding StandardCharsets/US_ASCII)
  (base64-basic-encoding StandardCharsets/UTF_8)
  (is (= (hex-string->base64-string "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
         "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")))
