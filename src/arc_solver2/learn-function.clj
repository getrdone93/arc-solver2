(ns arc-solver2.learn-function
  (:require [arc-solver2.core :as core]))

;;(require '(arc-solver2 [learn-function :as lf]))

(defn shape [image]
  [(count image), (count (first image))])

(defn pix-diff
  [i1 i2]
  (if (= (shape i1) (shape i2))
    (reduce (fn [av r]
            (+ av (apply + r))) 0 (mapv (fn [r1 r2]
                                          (mapv (fn [rc1 rc2]
                                                  (if (= rc1 rc2)
                                                    0
                                                    1)) r1 r2)) i1 i2))
    nil))

(defn build-funcs
  [two-ary unary]
   (map (fn [[taf [f1 f2]]]
          (cond
            (= taf concat) (fn [row] (taf (flatten (vector (f1 row)))
                                          (flatten (vector (f2 row)))))
            :else nil))
        (core/cartesian-product two-ary (core/cartesian-product unary unary))))

(defn invoke-func
  [f image]
  (mapv #(vec (f %)) image))

(defn find-funcs
  ([fs in-img out-img] (find-funcs fs in-img out-img (pix-diff in-img out-img) []))
  ([fs in-img out-img diff keep-fs]
   (keep (fn [[f i pd]]
             (when (and (some? pd) (or (< pd diff) (zero? pd)))
               [f i pd])) (map (fn [f]
                               (let [i (invoke-func f in-img)]
                                 [f i (pix-diff i out-img)])) fs))))
