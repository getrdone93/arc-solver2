(ns arc-solver2.image-transforms
  (:require [arc-solver2.image-utils :as iu]))

;;All functions herein return a single or multiple images

(defn transpose
  [image]
  (apply mapv vector image))

(defn rotate-image
  [image]
  (apply mapv (fn [& args]
                (vec (reverse args))) image))

(defn make-image
  ([r c] (make-image r c 0))
  ([r c v] (vec (take r (repeat (vec (take c (repeat v))))))))

(defn vert-image-halves
  [image]
  (let [mid (quot (count (first image)) 2)]
    (mapv (fn [[s e]]
            (mapv (fn [row]
                    (subvec row s e)) image))
          [[0 mid] [(inc mid) (count (first image))]])))

(defn split-along
  [val image]
  (mapv (fn [img]
          (mapv #(vec %) img)) (filter (fn [sub-img]
                                         (not= (-> sub-img first first) val))
                                       (apply concat (map (fn [grp-rows]
                                                            (apply map (fn [& args]
                                                                         (vec args)) grp-rows))
                                                          (partition-by #(= (count %) 1)
                                                                        (map (fn [row]
                                                                               (partition-by #(= % val) row)) image)))))))

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

(defn filter-image
  [keep bg image]
  (transpose
    (apply mapv (fn [& col]
                  (mapv (fn [cv]
                          (if (= cv keep)
                            keep
                            bg)) col)) image)))

(defn color-layers
  [bg image]
  (mapv (fn [keep]
       (filter-image keep bg image)) (iu/colors image bg)))

(defn color-cols
  [image col-colors]
  (transpose (color-rows (transpose image) col-colors)))

(defn replicate-pixel
  [[fr fc] to image]
  (assoc-in image to (-> image (nth fr) (nth fc))))

(defn replace-pixel
  [from-val from to image]
  (assoc-in (replicate-pixel from to image) from from-val))

(defn transfer-portion
  [[br er] [bc ec] rs cs pix-func image]
  (let [coords (iu/cartesian-product (range br er) (range bc ec))
        s-coords (iu/cartesian-product (range (+ br rs) (+ er rs))
                                       (range (+ bc cs) (+ ec cs)))]
    (reduce (fn [img [c sc]]
              (pix-func c sc img)) image
            (apply mapv (fn [c sc] [c sc]) [coords s-coords]))))

(defn replicate-portion
  [rows cols rs cs image]
  (transfer-portion rows cols rs cs replicate-pixel image))

(defn replace-portion
  ([rows cols rs cs image]
  (replace-portion rows cols (partial replace-pixel 0) rs cs image))
  ([rows cols pix-func rs cs image]
   (transfer-portion rows cols rs cs pix-func image)))

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

(defn transform-and-merge
  ([image ops bd-mrgf]
   (transform-and-merge image ops 1 bd-mrgf))
  ([image ops times bd-mrgf]
   (if (zero? times)
     image
     (transform-and-merge (bd-mrgf image (reduce (fn [im f]
                                               (f im)) image ops))
                          ops (dec times) bd-mrgf))))

(defn swap-pixels
  [image [r1 c1] [r2 c2]]
  (assoc-in (assoc-in image [r1 c1] (-> image (nth r2) (nth c2)))
            [r2 c2] (-> image (nth r1) (nth c2))))

(defn fractal
  [image]
  (let [rep-img (mapv (fn [r]
                        (mapv (fn [x] (if (zero? x)
                                        (make-image (count image) (count (first image)))
                                        image)) r)) image)]
    (mapv (fn [[r sr]]
            (vec (apply concat (map (fn [row]
                                      (row sr)) (rep-img r)))))
          (iu/cartesian-product (range (count rep-img)) (range (count rep-img))))))

(defn repeat-half
  [image]
  (mapv (fn [r]
          (mapv #(if (zero? %)
                   0
                   (inc %)) r)) (vec (drop-last (/ (count image) 2)
                                                (replicate-image image 1 0)))))

(defn half-intersection
  [image]
  (let [[lh rh] (vert-image-halves image)]
    (mapv (fn [l r]
            (mapv (fn [le re]
                    (if (> (first (set [le re])) 0)
                      (inc le)
                      0)) l r)) lh rh)))

(defn color-bars
  [image]
  (let [crs (sort-by last (filter (fn [[_ _ n]]
                                    (some? n))
                                  (map-indexed (fn [i r]
                                                 [i r ((frequencies r) 5)])
                                               (transpose image))))
        ;;y,g,r,b
        cols {0 4 1 3 2 2 3 1}]
    (apply color-cols (concat [image]
                                  [(vec (map-indexed (fn [i [ri r]]
                                                       (let [inds (iu/indicies-of r 5)]
                                                         [ri [(first inds) (inc (last inds))] (cols i)]))
                                                     crs))]))))

(defn crop
  [image]
  (let [[[rb re] [cb ce]] (iu/shape-coords (iu/non-bg-locs image 0))]
    (slice-image image [rb (inc re)] [cb (inc ce)])))

(defn smash-cols
  [image]
  (transpose (mapv #(iu/group-colors-by-row % 0 0) (transpose image))))