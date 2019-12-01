(ns io.github.greenrecyclebin.cryptopals.test-utils
  (:import
   (java.nio.file Path))

  (:require
   [clojure.java.io :as io]))

(declare relative-to-root-resource)

(defn root-directory
  "Returns the root resource path for a lib"
  [lib]
  (let [d (relative-to-root-resource lib)]
    (subs d 0 (.lastIndexOf d "/"))))

(defn- relative-to-root-resource
  "Returns the root directory path for a lib"
  {:tag String}
  [lib]
  (str (.. (name lib)
           (replace \- \_)
           (replace \. \/))))

(declare path-to-file)

(defn path-to-resource [s root-directory-of-ns]
  (-> s
      (path-to-file root-directory-of-ns)
      io/resource))

(defn path-to-file [s root-directory-of-ns]
  (-> root-directory-of-ns
      (Path/of (into-array [s]))
      str))
