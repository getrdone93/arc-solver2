(ns arc-solver2.core
  (:gen-class)
  ;keep for REPL
  (:require [arc-solver2.pixel-transforms :as ptf])
  (:require [arc-solver2.shape-transforms :as stf])
  (:require [arc-solver2.image-utils :as iu])
  (:require [arc-solver2.collection-transforms :as ctf])
  (:require [arc-solver2.search :as search])
  ;keep for REPL
  (:require [cheshire.core :as chesh-core])
  (:require [clojure.tools.logging :as log]))

(def arc-data "/home/tanderson/git/ARC/data/")

(def train-path (str arc-data "training/"))

(defn problem-path
  [problem]
  (str train-path problem))

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

(defn all-problems
  [data-path]
  (let [[_ & prob-files] (file-seq (clojure.java.io/file data-path))]
    (map (fn [f]
           [(. f getAbsolutePath) (chesh-core/parse-string (slurp f))]) prob-files)))

(defn func-on-all-probs
  [func all-probs]
  (reduce (fn [[tot-solv tot] [_ task-solv task-tot]]
            [(+ tot-solv task-solv) (+ tot task-tot)])
          [0 0] (map (fn [[fp {train "train"}]]
                       [fp (apply + (map (fn [{in "input" out "output"}]
                                           (if (= (iu/shape in) (iu/shape out))
                                             1
                                             (let [res (func in out)]
                                               (if (empty? res)
                                                 0
                                                 1)))) train)) (count train)]) all-probs)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
