(ns arc-solver2.disk-utils
  (:require [cheshire.core :as chesh-core]))

(defn all-problems
  [data-path]
  (let [[_ & prob-files] (file-seq (clojure.java.io/file data-path))]
    (map (fn [f]
           [(. f getAbsolutePath) (chesh-core/parse-string (slurp f))]) prob-files)))

;for repl and local runs
(def data-dir "/home/tanderson/git/ARC/data/")
;(def data-dir "/mnt/data/")

(def train-path (str data-dir "training/"))

(def probs (all-problems train-path))

(defn problem-path
  [problem]
  (str train-path problem))

(defn out-path
  [file-name]
  (str "/home/tanderson/git/arc-solver2/output/" file-name))

(defn find-problems
  ([prob-name]
   (find-problems probs prob-name))
  ([probs prob-name]
   (filterv (fn [[fp _]]
              (.contains fp prob-name)) probs)))

(defn read-problem
  [problem-name]
  (chesh-core/parse-string (slurp (problem-path problem-name))))

(defn write-solution
  [out-path structure]
  (spit out-path (chesh-core/generate-string structure)))
