(ns arc-solver2.search
  (:require [arc-solver2.image-utils :as iu])
  (:require [arc-solver2.shape-transforms :as stf])
  (:require [clojure.tools.logging :as log]
            [arc-solver2.pixel-transforms :as ptf]))

;(ns-publics 'arc-solver2.image-transforms)

;here is the interpreter, its shorter than arc1's
;(conj '(color-test first second inc inc) '->)

(defn repeat-token
  ([[in prog :as img-prog] out token diff-func]
   (let [diff (diff-func in out)]
     (if (zero? diff)
       [img-prog diff]
       (let [nimg (token in)
             n-diff (diff-func nimg out)]
         (cond
           (zero? n-diff) [[nimg (conj prog token)] n-diff]
           (>= n-diff diff) [img-prog diff]
           :else (repeat-token [nimg (conj prog token)] out token diff-func)))))))

(defn look-ahead
  [frontier out max-diff diff-func]
  (let [new-front (map (fn [[_ prog :as ip]]
                         (repeat-token ip out (last prog) diff-func)) frontier)
        perfect (filter (fn [[_ diff]]
                          (zero? diff)) new-front)
        rem-diff (fn [front]
                   (mapv #(vec (first %)) front))]
    (if (empty? perfect)
      [:better (rem-diff (sort
                           (fn [[_ d1] [_ d2]]
                             (< d1 d2))
                           (filter (fn [[_ d]]
                                     (< d max-diff)) new-front)))]
      [:perfect (rem-diff perfect)])))

(def counter (atom 0))
(def min-diff (atom Long/MAX_VALUE))

(defn greedy-limited-bfs
  ([in out tokens diff-func]
   (if (and (iu/exists? out) (iu/exists? in) (pos? (diff-func in out)))
     (greedy-limited-bfs [[in []]] out tokens Long/MAX_VALUE {} diff-func)
     []))
  ([[[in prog] & res-ins :as frontier] out tokens max-depth all-progs diff-func]
   (cond
     (and (not (iu/exists? in)) (empty? res-ins)) (into [] all-progs)
     (>= (count prog) max-depth) (recur res-ins out tokens max-depth all-progs diff-func)
     (iu/exists? in) (let [d (diff-func in out)
                           [front-key new-front] (look-ahead (mapv (fn [tok]
                                                                     [(tok in) (conj prog tok)]) tokens)
                                                             out d diff-func)]
                       (swap! counter inc)
                       (swap! min-diff #(min % d))
                       (when (zero? (mod (deref counter) 1000))
                         (let [pls (map (fn [[_ prog]]
                                          (count prog)) frontier)]
                           (println "(min,max,avg): " [(apply min pls) (apply max pls) (float (/ (apply + pls) (count pls)))]
                                    " frontier size: " (inc (count res-ins)) "min diff: " (deref min-diff))))
                       (if (= front-key :perfect)
                         (recur res-ins out tokens
                                             (inc (count prog)) (merge (into {} new-front) all-progs) diff-func)
                         (recur (vec (concat res-ins
                                                          (mapv (fn [[k vs]]
                                                                  [k (second (first vs))])
                                                                (group-by first new-front))))
                                             out tokens max-depth all-progs diff-func)))
     :else (recur res-ins out tokens max-depth all-progs diff-func))))

(defn equal-shape
  [in out]
  (greedy-limited-bfs in out (iu/funcs-for-ns 'arc-solver2.shape-transforms stf/func-space) iu/shape-diff))

(defn equal-image
  [in out]
  (greedy-limited-bfs in out (iu/funcs-for-ns 'arc-solver2.pixel-transforms ptf/func-space) iu/pix-diff))

(defn solve-pair
  [in out]
  (let [diff (iu/diff in out)]
    (if (nil? (:pix diff))
      (greedy-limited-bfs (equal-shape in out) out
                          (iu/funcs-for-ns 'arc-solver2.pixel-transforms ptf/func-space)
                          Long/MAX_VALUE
                          {}
                          iu/pix-diff)
      (equal-image in out))))