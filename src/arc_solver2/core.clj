(ns arc-solver2.core
  (:gen-class)
  (:require [arc-solver2.image-transforms :as itf])
  (:require [arc-solver2.image-utils :as iu]) ;keep this for REPL
  (:require [cheshire.core :as chesh-core]))

(defn problem-path
  [problem]
  (str "/home/tanderson/git/ARC/data/training/" problem))

(defn out-path
  [file-name]
  (str "/home/tanderson/git/arc-solver2/output/" file-name))

(defn read-problem
  [problem-name]
  (chesh-core/parse-string (slurp (problem-path problem-name))))

(defn test-train-image
  [problem n]
  ((first (problem "train")) "input"))

(defn write-solution
  [out-path structure]
  (spit out-path (chesh-core/generate-string structure)))

(defn solve-problem
  [input sol-func]
  (reduce (fn [iv k]
            (assoc iv k (mapv (fn [{i "input"}]
                                {"input" i "output" (sol-func i)})
                              (input k)))) {} (keys input)))

(def problem-solution {"007bbfb7.json" itf/fractal
                       "017c7c7b.json" itf/repeat-half ;;mysterious no reverse on first train example
                       "0520fde7.json" itf/half-intersection
                       "08ed6ac7.json" itf/color-bars
                       "1cf80156.json" itf/crop
                       "1e0a9b12.json" itf/smash-cols})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
