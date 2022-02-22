(ns arc-solver2.core
  (:gen-class)
  ;arc-solver2 ns
  (:require [arc-solver2.pixel-transforms :as ptf])
  (:require [arc-solver2.shape-transforms :as stf])
  (:require [arc-solver2.image-utils :as iu])
  (:require [arc-solver2.collection-transforms :as ctf])
  (:require [arc-solver2.merge-transform :as mtf])
  (:require [arc-solver2.search :as search])
  (:require [arc-solver2.evolutionary-search :as es])
  (:require [arc-solver2.disk-utils :as du])

  ;solutions
  (:require [arc-solver2.solution-746b3537 :as sol1])

  ;keep for REPL
  (:require [clojure.tools.logging :as log])
  (:require [clojure.java.io :as io])
  (:require [clojure.edn :as edn]))

(import '(java.util.concurrent Executors))

(defn test-train-image
  [problem n]
  ((first (problem "train")) "input"))

(defn solve-problem
  [input sol-func]
  (reduce (fn [iv k]
            (assoc iv k (mapv (fn [{i "input"}]
                                {"input" i "output" (sol-func i)})
                              (input k)))) {} (keys input)))

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
     (Thread/sleep (* 1000 timeout))
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
  [{all-probs :all-probs search-func :search-func num-thrds :num-thrds
    thrd-pool :thrd-pool shut-down-atom :shut-down-atom timeout :timeout}]
  (reduce (fn [[slv-tsk tot-tsk] [ind fp fgrps tcnt]]
            (do
              (println "sa: " (type search-func) "ind: " ind " problem: " fp " " (fmt-percentage (inc ind) (count all-probs))
                       " total solved: " slv-tsk " total tasks: " tot-tsk
                       ;" pct solv: " (fmt-percentage slv-tsk tot-tsk)
                       )
              (let [res (filter (fn [[_ {s :solved}]]
                                  s) (apply concat (map #(exec-funcs % timeout thrd-pool shut-down-atom) fgrps)))]
                [(+ slv-tsk (count res))
                 (+ tot-tsk tcnt)])))
          [0 0]
          (map-indexed (fn [ind [fp {tsks "train"}]]
                         [ind fp (partition num-thrds
                                            (map (fn [{in "input" output "output"}]
                                                   (partial search-func in output)) tsks)) (count tsks)]) all-probs)))



(def search-func-map
  {:greedy-limited-bfs {:search search/find-progs
                        :atom search/shutting-down}
   :genetic-algorithm  {:search es/search
                        :atom es/shutting-down}})

(defn -main
  [& args]
  (println "running arc-solver2, found " (count du/probs) " problems under " du/train-path)
  (let [vec-args (vec args)]
    (if (= (count vec-args) 1)
      (let [file-res (io/resource (str "search/" (first vec-args) ".edn"))]
        (if (some? file-res)
          (let [props (edn/read-string (slurp file-res))
                thrd-pool (Executors/newFixedThreadPool (props :threads))]
            (solve-all-tasks {:all-probs du/probs
                              :search-func ((search-func-map (props :search-name)) :search)
                              :num-thrds (props :threads)
                              :thrd-pool thrd-pool
                              :shut-down-atom ((search-func-map (props :search-name)) :atom)
                              :timeout (props :timeout)})
            (doall (map (fn [[_ {a :atom}]]
                          (reset! a true)) search-func-map))
            (.shutdownNow thrd-pool))
          (println "error: " (first vec-args) " was not found\n"
                   "usage: java -jar [executable-name] [prop-file-name]")))
      (println "usage: java -jar [executable-name] [prop-file-name]"))))
