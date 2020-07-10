(ns arc-solver2.image-transforms
  (:require [arc-solver2.image-utils :as iu]))

;;All functions herein return a single or multiple images

(defn transpose
  [image]
  (apply mapv vector image))

(defn make-image
  ([r c] (make-image r c 0))
  ([r c v] (vec (take r (repeat (vec (take c (repeat v))))))))

(defn cartesian-product
  [m n]
  (vec (apply concat (map (fn [mi]
                            (mapv (fn [ni]
                                    [mi ni]) n)) m))))

(defn vert-image-halves
  [image]
  (let [mid (quot (count (first image)) 2)]
    (mapv (fn [[s e]]
            (mapv (fn [row]
                    (subvec row s e)) image))
          [[0 mid] [(inc mid) (count (first image))]])))

(defn row-swap
  [image ind-rows s]
  (apply assoc (iu/row-swap-args image ind-rows s)))

(defn col-swap
  [image ind-rows s]
  (transpose (apply assoc (iu/row-swap-args (transpose image) ind-rows s))))

(defn color-rows
  [image row-colors]
  (apply assoc (vec (reduce (fn [av [r [b e] c]]
                         (concat av [r (apply assoc (vec (concat [(image r)]
                                                   (flatten (map #(vector % c) (range b e))))))]))
                            [image]
                       row-colors))))

(defn color-cols
  [image col-colors]
  (transpose (color-rows (transpose image) col-colors)))

(defn replicate-image
  [image r c]
  (mapv (fn [r]
          (vec (apply concat (concat [r] (repeat c r)))))
        (vec (apply concat (concat [image] (repeat r image))))))

(defn slice-image
  [image [rb re] [cb ce]]
  (mapv #(subvec % cb ce)
   (subvec image rb re)))

(defn shift-rows
  ([image shift-func]
   (shift-rows image (set (range (count image))) shift-func))
  ([image rows shift-func]
  (vec (map-indexed (fn [i r]
                      (if (nil? (rows i))
                        r
                        (shift-func r))) image))))

(defn shift-rows-left
  ([image]
   (shift-rows image iu/shift-left))
  ([image rows]
   (shift-rows image rows iu/shift-left)))

(defn shift-rows-right
  ([image]
   (shift-rows image iu/shift-right))
  ([image rows]
   (shift-rows image rows iu/shift-right)))

(defn shift-cols
  ([image shift-func]
   (transpose (shift-rows (transpose image) shift-func)))
  ([image cols shift-func]
   (transpose (shift-rows (transpose image) cols shift-func))))

(defn shift-cols-down
  ([image]
   (shift-cols image iu/shift-right))
  ([image cols]
   (shift-cols image cols iu/shift-right)))

(defn shift-cols-up
  ([image]
   (shift-cols image iu/shift-left))
  ([image cols]
   (shift-cols image cols iu/shift-left)))

(defn merge-images
  [merge-func bg img1 img2]
  (mapv (fn [ir1 ir2]
          (mapv (fn [p1 p2]
                  (merge-func p1 p2 bg)) ir1 ir2)) img1 img2))

(defn shift-and-merge
  ([image ops bd-mrgf]
   (shift-and-merge image ops 1 bd-mrgf))
  ([image ops times bd-mrgf]
   (if (zero? times)
     image
     (shift-and-merge (bd-mrgf image (reduce (fn [im f]
                                               (f im)) image ops))
                      ops (dec times) bd-mrgf))))

(defn swap-pixels
  [image [r1 c1] [r2 c2]]
  (assoc-in (assoc-in image [r1 c1] (-> image (nth r2) (nth c2)))
            [r2 c2] (-> image (nth r1) (nth c2))))

