(ns arc-solver2.pixel-transforms
  (:require [arc-solver2.image-utils :as iu]))

(defn rotate-image
  [image]
  (apply mapv (fn [& args]
                (vec (reverse args))) image))

(defn filter-image
  [keep bg image]
  (iu/filter-image #{keep} bg image))

;(defn overlay
;  [bg images]
;  (iu/merge-img-coll (partial iu/merge-images (fn [p1 p2 _]
;                                          (max p1 p2))) bg images))

(defn shift-rows-left
  ([image]
   (iu/shift-rows image iu/shift-left))
  ([image rows]
   (iu/shift-rows image rows iu/shift-left)))

(defn shift-rows-right
  ([image]
   (iu/shift-rows image iu/shift-right))
  ([image rows]
   (iu/shift-rows image rows iu/shift-right)))

(defn shift-cols-down
  ([image]
   (iu/shift-cols image iu/shift-right))
  ([image cols]
   (iu/shift-cols image cols iu/shift-right)))

(defn shift-cols-up
  ([image]
   (iu/shift-cols image iu/shift-left))
  ([image cols]
   (iu/shift-cols image cols iu/shift-left)))

(def func-space
  {'filter-image (iu/func-space filter-image (map #(vector % 0) (range 10)))})