(ns arc-solver2.core
  (:gen-class)
  ;keep for REPL
  (:require [arc-solver2.pixel-transforms :as ptf])
  (:require [arc-solver2.shape-transforms :as stf])
  (:require [arc-solver2.image-utils :as iu])
  (:require [arc-solver2.collection-transforms :as ctf])
  (:require [arc-solver2.merge-transform :as mtf])
  (:require [arc-solver2.search :as search])
  ;keep for REPL
  (:require [cheshire.core :as chesh-core])
  (:require [clojure.tools.logging :as log]))

(import '(java.util.concurrent Executors))

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

(defn func-on-all-probs-new
  [func all-probs]
  (reduce (fn [[tot-solv tot] [_ task-solv task-tot]]
            [(+ tot-solv task-solv) (+ tot task-tot)])
          [0 0] (map-indexed (fn [ind [fp {train "train"}]]
                               (println "solving " ind fp (str (format "%.2f"
                                                                   (* 100 (float (/ (inc ind) (count all-probs))))) "%"))
                               [fp (apply + (map (fn [{in "input" out "output"}]
                                                   (let [{pd :pix} (iu/diff in out)]
                                                     (if (and (some? pd) (zero? pd))
                                                       1
                                                       (let [res (func in out)]
                                                         (if (empty? res)
                                                           0
                                                           1))))) train)) (count train)]) all-probs)))

(defn thread-name
  []
  (. (. Thread currentThread) getName))

(def tasks
  (repeat 3 (fn []
              (do
                (println (str (. (. Thread currentThread) getName) " is sleeping..."))
                (Thread/sleep 500)
                (str "hello from " (. (. Thread currentThread) getName))))))

(defn submit-tasks-to-pool
  ([pool tasks]
   (submit-tasks-to-pool pool tasks []))
  ([pool [t & ts] res]
   (if (some? t)
     (recur pool ts (conj res (.submit pool t)))
     res)))

(defn new-schedule-example
  []
  (let [pool (Executors/newFixedThreadPool 3)
        tasks (submit-tasks-to-pool pool tasks)]
    (println (thread-name) " is going to sleep...")
    (Thread/sleep 3000)
    (doseq [future tasks]
      (if (.isDone future)
        (println "no cancel, future value: " (.get future))
        (do
          (println "cancelling future")
          (.cancel future true))))
    (.shutdown pool)))

(def probs (all-problems train-path))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
