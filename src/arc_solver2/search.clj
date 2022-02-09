(ns arc-solver2.search
  (:require [arc-solver2.image-utils :as iu])
  (:require [arc-solver2.shape-transforms :as stf])
  (:require [arc-solver2.custom-space :as cs])
  (:require [clojure.tools.logging :as log]
            [arc-solver2.pixel-transforms :as ptf])
  (:require [cheshire.core :as chesh-core]))

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
(def best-image (atom (iu/make-image 1 1)))

(def test-frontier (atom []))
(def out-image (atom []))

;bad input b190f7f5.json

(defn write-solution
  [out-path structure]
  (spit out-path (chesh-core/generate-string structure)))

(defn debug-search [{d :dist in :in out :out frontier :frontier res-ins :res-ins}]
  (swap! counter inc)
  (when (< d @min-diff)
    (reset! min-diff d)
    (reset! best-image in)
    (reset! test-frontier frontier)
    (reset! out-image out))
  (when (zero? (mod (deref counter) 1000))
    (let [pls (map (fn [[_ prog]]
                     (count prog)) frontier)
          data {"train" (mapv (fn [img]
                                {"input" img "output" out})
                              (map first (take 10 frontier)))
                "test" [{"input" @best-image "output" [[1]]}]}]

      (println "(min,max,avg): " [(apply min pls) (apply max pls) (float (/ (apply + pls) (count pls)))]
               " frontier size: " (inc (count res-ins)) " min diff: " (deref min-diff))
      (write-solution "/home/tanderson/git/arc-solver2/output/viz.json" data))))

(def shutting-down (atom false))

(defn greedy-limited-bfs
  ([in out tokens diff-func shut-down]
   (if (and (iu/valid-image? out) (iu/valid-image? in) (pos? (diff-func in out)))
     (do
       (reset! min-diff Long/MAX_VALUE)
       (greedy-limited-bfs (sorted-set-by (fn [[img1 _] [img2 _]]
                                            (< (diff-func img1 out) (diff-func img2 out))) [in []])
                           out tokens Long/MAX_VALUE {} diff-func shut-down))
     []))
  ([[[in prog] & res-ins :as frontier] out tokens max-depth all-progs diff-func shut-down]
     (if @shut-down
       (let [srted (apply sorted-set-by (fn [[img1 _] [img2 _]]
                                          (< (diff-func img1 out) (diff-func img2 out))) all-progs)]
         {:solved   (= (-> srted first first) out)
          :programs srted})
       (cond
         (and (not (iu/valid-image? in)) (empty? res-ins)) (let [srted (apply sorted-set-by
                                                                              (fn [[img1 _] [img2 _]]
                                                                                (< (diff-func img1 out) (diff-func img2 out))) all-progs)]
                                                             {:solved   (= (-> srted first first) out)
                                                              :programs srted})
         (>= (count prog) max-depth) (recur res-ins out tokens max-depth all-progs diff-func shut-down)
         (iu/valid-image? in) (let [d (diff-func in out)
                                    [front-key new-front] (look-ahead (mapv (fn [tok]
                                                                              [(tok in) (conj prog tok)]) tokens)
                                                                      out d diff-func)]
                                ;(debug-search {:dist d :in in :out out :frontier frontier :res-ins res-ins})
                                (if (= front-key :perfect)
                                  (recur res-ins out tokens
                                         (inc (count prog)) (clojure.set/union all-progs new-front) diff-func shut-down)
                                  (recur (clojure.set/union res-ins (mapv (fn [[_ vs]]
                                                                            (first (sort (fn [[_ p1] [_ p2]]
                                                                                           (< (count p1) (count p2))) vs)))
                                                                          (group-by first new-front)))
                                         out tokens max-depth all-progs diff-func shut-down)))
         :else (recur res-ins out tokens max-depth all-progs diff-func shut-down)))))

(defn equal-shape
  [in out]
  (greedy-limited-bfs in out (iu/funcs-for-ns 'arc-solver2.shape-transforms stf/func-space) iu/shape-diff shutting-down))

(defn equal-image
  [in out]
  (greedy-limited-bfs in out (iu/funcs-for-ns 'arc-solver2.pixel-transforms ptf/func-space) iu/pix-diff shutting-down))

(defn find-progs
  [in out]
  (greedy-limited-bfs in out (vec (shuffle (concat (iu/funcs-for-ns 'arc-solver2.shape-transforms stf/func-space)
                                                   (iu/funcs-for-ns 'arc-solver2.pixel-transforms ptf/func-space)
                                                   cs/func-space)))
                      iu/percent-diff shutting-down))

(defn solve-pair
  [in out]
  (let [diff (iu/diff in out)]
    (if (nil? (:pix diff))
      (let [eq-shps (equal-shape in out)
            _ (reset! min-diff (. Long MAX_VALUE))]
        (greedy-limited-bfs eq-shps out
                            (iu/funcs-for-ns 'arc-solver2.pixel-transforms ptf/func-space)
                            Long/MAX_VALUE
                            {}
                            iu/pix-diff shutting-down))
      (equal-image in out))))