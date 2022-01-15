(ns arc-solver2.shape-transforms
  (:require [arc-solver2.image-utils :as iu]))

(defn transpose
  [image]
  (iu/transpose image))

(defn replicate-horizontal
  [image]
  (iu/replicate-image image 1 0))

(defn replicate-vertical
  [image]
  (iu/replicate-image image 0 1))

(defn remove-first-row
  [image]
  (vec (rest image)))

(defn remove-last-row
  [image]
  (vec (drop-last image)))

(defn remove-last-col
  [image]
  (transpose (remove-last-row (transpose image))))

(defn remove-first-col
  [image]
  (transpose (remove-first-row (transpose image))))

(defn add-first-row
  [v image]
  (vec (concat [(iu/gen-row v image)] image)))

(defn add-last-row
  [v image]
  (conj image (iu/gen-row v image)))

(defn add-first-col
  [v image]
  (transpose (add-first-row v (transpose image))))

(defn add-last-col
  [v image]
  (transpose (add-last-row v (transpose image))))

(defn increase-border
  [v image]
  (reduce (fn [im f]
            (f v im)) image [add-first-col add-first-row
                             add-last-col add-last-row]))

(defn decrease-border
  [image]
  (let [[r c] (iu/shape image)]
    (if (or (< r 3) (< c 3))
      image
      (reduce (fn [im f]
                (f im)) image [remove-first-col remove-first-row
                               remove-last-col remove-last-row]))))

(defn crop
  [image]
  (let [bg-locs (iu/non-bg-locs image 0)]
    (if (empty? bg-locs)
      image
      (let [[[rb re] [cb ce]] (iu/shape-coords bg-locs)]
        (iu/slice-image image [rb (inc re)] [cb (inc ce)])))))

(def func-space
  {'add-first-row   (iu/color-func-space add-first-row)
   'add-last-row    (iu/color-func-space add-last-row)
   'add-first-col   (iu/color-func-space add-first-col)
   'add-last-col    (iu/color-func-space add-last-col)
   'increase-border (iu/color-func-space increase-border)
   'remove-first-col [remove-first-col]
   'remove-first-row [remove-first-row]
   'remove-last-col [remove-last-col]
   'remove-last-row [remove-last-row]
   'decrease-border [decrease-border]})
