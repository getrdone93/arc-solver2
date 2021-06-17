(ns arc-solver2.merge-transform
  (:require [arc-solver2.image-utils :as iu]))

(defn general-row-merge
  [min-max-col pad-val take-func images]
  (let [ci (count images)]
    (cond
      (or (neg? ci) (zero? ci)) []
      (= 1 (count images)) (first images)
      :else (let [cols (apply min-max-col (map #(second (iu/shape %)) images))]
              (mapv (fn [row]
                      (let [rd (- (count row) cols)]
                        (vec (cond
                               (zero? rd) row
                               (pos? rd) (take-func cols row)
                               (neg? rd) (concat row (repeat (- rd) pad-val))))))
                    (apply concat images))))))