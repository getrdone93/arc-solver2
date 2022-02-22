(ns arc-solver2.evolutionary-search
  (:require [arc-solver2.image-utils :as iu]
            [arc-solver2.shape-transforms :as stf]
            [arc-solver2.pixel-transforms :as ptf]
            [arc-solver2.custom-space :as cs]))

(def token-space
  (set (shuffle (concat (iu/funcs-for-ns 'arc-solver2.shape-transforms stf/func-space)
                        (iu/funcs-for-ns 'arc-solver2.pixel-transforms ptf/func-space)
                        cs/func-space))))

(defn sort-images-with-diff
  [imgs out-img]
  (sort (fn [[_ d1] [_ d2]]
          (< d1 d2))
        (map (fn [img]
               [img (iu/percent-diff img out-img)]) imgs)))

(defn sort-images
  [imgs out-img]
  (map first (sort-images-with-diff imgs out-img)))

(defn evolve
  ([in-img pool-size tokens]
   (evolve (repeat pool-size in-img) tokens))
  ([imgs tokens]
   (filter (fn [img]
             (iu/valid-image? img))
           (map (fn [img t]
                  (t img)) imgs (take (count imgs) (shuffle tokens))))))

(defn select-first-last
  [imgs out-img]
  (let [srted (sort-images imgs out-img)]
    (shuffle (apply concat [[(first srted)] [(last srted)]
                            (take (- (count imgs) 2) (shuffle srted))]))))

(defn select-top-gene
  [imgs out-img]
  (let [srted (sort-images imgs out-img)
        half (/ (count srted) 2)]
    (shuffle (repeat (count srted) (first srted)))))

(defn select
  [imgs out-img]
  (let [srted (sort-images imgs out-img)
        half (/ (count srted) 2)]
    (shuffle (apply concat [(repeat half (first srted))
                            (take half (shuffle srted))]))))

(def shutting-down (atom false))

(defn search
  ([in-img out-img]
   (search (select (evolve in-img 50 token-space) out-img) out-img 0 shutting-down))
  ([imgs out-img gen sd]
   (let [img-diffs (sort-images-with-diff imgs out-img)
         srted (map first img-diffs)]
     (if @sd
       {:solved (= (first srted) out-img)
        :programs srted
        :misc {:gen gen}}
       (do
         ;(when (zero? (mod gen 10))
         ;  (println "gen " gen " diffs: " (map (fn [i]
         ;                                        (float (iu/percent-diff i out-img))) srted)))
         (if (= (first srted) out-img)
           {:solved (= (first srted) out-img)
            :programs srted
            :misc {:gen gen}}
           (recur (select (evolve srted token-space) out-img) out-img (inc gen) sd)))))))