(ns arc-solver2.merge-transform
  (:require [arc-solver2.image-utils :as iu]))

(defn general-row-merge
  [min-max-col pad-val take-func images]
  (let [ci (count images)]
    (cond
      (or (neg? ci) (zero? ci)) [[]]
      (= 1 ci) (first images)
      :else (let [img-shps (filter (fn [[shp img]]
                                     (when (some? shp)
                                       [shp img])) (map (fn [i]
                                                          [(iu/shape i) i]) images))]
              (if (empty? img-shps)
                [[]]
                (let [filt-imgs (map second img-shps)
                      filt-cols (map (fn [[shp _]]
                                       (second shp)) img-shps)
                      cols (apply min-max-col filt-cols)]
                  (mapv (fn [row]
                          (let [rd (- (count row) cols)]
                            (vec (cond
                                   (zero? rd) row
                                   (pos? rd) (take-func cols row)
                                   (neg? rd) (concat row (repeat (- rd) pad-val))))))
                        (apply concat filt-imgs))))))))

(def func-space
  {'general-row-merge (for [func [min max]
                            pv (range 10)]
                        (partial general-row-merge func pv take))})