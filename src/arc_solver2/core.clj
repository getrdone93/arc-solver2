(ns arc-solver2.core
  (:gen-class)
  ;keep for REPL
  (:require [arc-solver2.pixel-transforms :as ptf])
  (:require [arc-solver2.shape-transforms :as stf])
  (:require [arc-solver2.image-utils :as iu])
  (:require [arc-solver2.collection-transforms :as ctf])
  (:require [arc-solver2.merge-transform :as mtf])
  (:require [arc-solver2.search :as search])
  (:require [arc-solver2.evolutionary-search :as es])
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

(defn exec-funcs
  ([funcs thrd-pool shut-down]
   (exec-funcs funcs (* 1000 15) thrd-pool shut-down))
  ([funcs timeout thrd-pool shut-down]
   (let [subm-tsks (do
                     (reset! shut-down false)
                     (doall (map (fn [f]
                                 (.submit thrd-pool f)) funcs)))]
     ;(println (thread-name) " will wait for " timeout "ms")
     (Thread/sleep timeout)
     (reset! shut-down true)
     (Thread/sleep 100)
     ;(println (thread-name) " collecting or cancelling")
     (doall (map (fn [future]
                   [future (.get future)]) subm-tsks)))))

(defn percentage
  [num denom]
  (if (zero? denom)
    0
    (* 100 (float (/ num denom)))))

(defn fmt-percentage
  [num denom]
  (str (format "%.2f" (percentage num denom)) "%"))

(def pool (Executors/newFixedThreadPool 2))

(defn solve-all-tasks
  ([problems thrd-pool search-func shut-down-atom]
   (solve-all-tasks problems search-func 2 thrd-pool shut-down-atom))
  ([all-probs search-func num-thrds thrd-pool shut-down-atom]
  (reduce (fn [[slv-tsk tot-tsk] [ind fp fgrps tcnt]]
            (do
              (println "ind: " ind " problem: " fp " " (fmt-percentage (inc ind) (count all-probs))
                       " total solved: " slv-tsk " total tasks: " tot-tsk
                       ;" pct solv: " (fmt-percentage slv-tsk tot-tsk)
                       )
              (let [res (filter (fn [[_ {s :solved}]]
                                  s) (apply concat (map #(exec-funcs % thrd-pool shut-down-atom) fgrps)))]
                [(+ slv-tsk (count res))
                 (+ tot-tsk tcnt)])))
          [0 0]
          (map-indexed (fn [ind [fp {tsks "train"}]]
                         [ind fp (partition num-thrds
                                            (map (fn [{in "input" output "output"}]
                                                   (partial search-func in output)) tsks)) (count tsks)]) all-probs))))

(def probs (all-problems train-path))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
