(ns io.github.greenrecyclebin.cryptopals.golden-file-utils
  (:import
   (java.nio.charset StandardCharsets)
   (java.nio.file Files)
   (java.util Base64))

  (:require
   [clojure.java.io :as io]

   [io.github.greenrecyclebin.cryptopals.test-utils :refer
    [path-to-file path-to-resource root-directory]]))

(def ^:private root-directory-of-ns (root-directory (ns-name *ns*)))

(declare encoded-with-basic-filename plaintext-filename)

(defn create-encoded-with-basic-golden-file-if-needed [charset-name]
  (when-not (-> charset-name
                encoded-with-basic-filename
                (path-to-resource root-directory-of-ns))
    (let [encoded-with-basic-file
          (io/file "test"
                   (-> charset-name
                       encoded-with-basic-filename
                       (path-to-file root-directory-of-ns)))

          file-created (.createNewFile encoded-with-basic-file)]

      (try
        (let [basic-encoder (Base64/getEncoder)]
          (with-open [reader
                      (io/reader (-> charset-name
                                     plaintext-filename
                                     (path-to-resource root-directory-of-ns)))

                      writer
                      (io/writer (-> charset-name
                                     encoded-with-basic-filename
                                     (path-to-resource root-directory-of-ns)))]
            (doseq [line (line-seq reader)
                    :let [bs (.getBytes line StandardCharsets/UTF_8)]]
              (.write writer (.encodeToString basic-encoder bs))
              (.write writer (System/lineSeparator)))))
        (catch Exception _
          (when file-created
            (.delete encoded-with-basic-file)))))))

(defn encoded-with-basic-filename [charset-name]
  (clojure.string/join "-" [charset-name "encoded-with-basic.txt"]))

(defn plaintext-filename [charset-name]
  (clojure.string/join "-" [charset-name "plaintext.txt"]))
