(ns arc-solver2.custom-space
  (:require [arc-solver2.collection-transforms :as ctf])
  (:require [arc-solver2.merge-transform :as mtf]))

(def func-space
  (for [coll-func (apply concat (vals ctf/func-space))
        merge-func (apply concat (vals mtf/func-space))]
    (partial (fn [cf mf image]
               (mf (cf image))) coll-func merge-func)))