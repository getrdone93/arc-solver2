(ns arc-solver2.image-utils)

(defn shape
  [image]
  [(count image) (-> image first count)])

(defn exists?
  [image]
  (some? (seq (flatten image))))

(defn row-swap-args
  [image ind-rows s]
  (concat [image] (apply concat (map (fn [r]
                                           [r (image (+ r s)) (+ r s) (image r)])
                                         ind-rows))))

(defn indicies-of
  [coll e]
  (keep-indexed (fn [i v]
                (when (= v e)
                  i)) coll))

(defn shift-right
  [row]
  (vec (concat [(last row)] (drop-last row))))

(defn shift-left
  [row]
  (vec (concat (rest row) [(first row)])))

(defn non-bg-locs
  [image bg]
  (filterv some? (apply concat (map-indexed (fn [r row]
                                             (map-indexed (fn [c cv]
                                                            (when (not= cv bg)
                                                              [r c cv])) row)) image))))

(defn shape-coords
  [non-bg-locs]
  (let [rows (map first non-bg-locs)
        cols (map second non-bg-locs)]
    [[(apply min rows) (apply max rows)] [(apply min cols) (apply max cols)]]))

(defn colors
  [image bg]
  (clojure.set/difference (set (flatten image)) #{bg}))

(defn group-colors-by-row
  [row bg order]
  (let [freqs (frequencies row)
        nb-fs (dissoc freqs bg)
        color-pix (apply concat (map (fn [[pix n]]
                                      (repeat n pix)) (sort nb-fs)))
        bg-pix (repeat (freqs bg) bg)]
    (vec (apply concat (cond (= order 1) [color-pix bg-pix]
                             (= order 0) [bg-pix color-pix])))))

(defn cartesian-product
  [m n]
  (vec (apply concat (map (fn [mi]
                            (mapv (fn [ni]
                                    [mi ni]) n)) m))))

(defn abs
  [n]
  (max n (- n)))

(defn diff
  [img1 img2]
  (let [[[i1r i1c] [i2r i2c]] (mapv (fn [i]
                                      [(count i) (-> i first count)])
                                    [img1 img2])]
    (if (and (= i1r i2r) (= i1c i2c))
      {:pix (apply + (mapv (fn [r1 r2]
                             (apply + (mapv (fn [p1 p2]
                                              (if (= p1 p2)
                                                0
                                                1)) r1 r2))) img1 img2))}
      {:row (abs (- i1r i2r))
       :col (abs (- i1c i2c))})))

(defn shape-diff
  [img1 img2]
 (let [{p :pix rd :row cd :col} (diff img1 img2)]
   (if (some? p)
     0
     (+ rd cd))))

(defn pix-diff
  [img1 img2]
  (let [{p :pix} (diff img1 img2)]
    (if (some? p)
      p
      0)))

(defn gen-row
  [v image]
  (vec (repeat (-> image first count) v)))

(defn transpose
  [image]
  (if (exists? image)
    (apply mapv vector image)
    image))

(defn swap-pixels
  [image [r1 c1] [r2 c2]]
  (assoc-in (assoc-in image [r1 c1] (-> image (nth r2) (nth c2)))
            [r2 c2] (-> image (nth r1) (nth c2))))

(defn transfer-portion
  [[br er] [bc ec] rs cs pix-func image]
  (let [coords (cartesian-product (range br er) (range bc ec))
        s-coords (cartesian-product (range (+ br rs) (+ er rs))
                                       (range (+ bc cs) (+ ec cs)))]
    (reduce (fn [img [c sc]]
              (pix-func c sc img)) image
            (apply mapv (fn [c sc] [c sc]) [coords s-coords]))))

(defn replicate-pixel
  [[fr fc] to image]
  (assoc-in image to (-> image (nth fr) (nth fc))))

(defn replace-pixel
  [from-val from to image]
  (assoc-in (replicate-pixel from to image) from from-val))

(defn replicate-portion
  [rows cols rs cs image]
  (transfer-portion rows cols rs cs replicate-pixel image))

(defn replace-portion
  ([rows cols rs cs image]
   (replace-portion rows cols (partial replace-pixel 0) rs cs image))
  ([rows cols pix-func rs cs image]
   (transfer-portion rows cols rs cs pix-func image)))

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

(defn row-swap
  [image ind-rows s]
  (apply assoc (row-swap-args image ind-rows s)))

(defn col-swap
  [image ind-cols s]
  (transpose (apply assoc (row-swap-args (transpose image) ind-cols s))))

(defn make-image
  ([r c]
   (make-image r c 0))
  ([r c v]
   (vec (take r (repeat (vec (take c (repeat v))))))))

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

(defn shift-cols
  ([image shift-func]
   (transpose (shift-rows (transpose image) shift-func)))
  ([image cols shift-func]
   (transpose (shift-rows (transpose image) cols shift-func))))

(defn merge-images
  [merge-func bg img1 img2]
  (mapv (fn [ir1 ir2]
          (mapv (fn [p1 p2]
                  (merge-func p1 p2 bg)) ir1 ir2)) img1 img2))

(defn merge-img-coll
  [merge-imgs-f bg images]
  (reduce (fn [agg img]
            (merge-imgs-f bg agg img)) (first images) (rest images)))

(defn transform-and-merge
  ([image ops bd-mrgf]
   (transform-and-merge image ops 1 bd-mrgf))
  ([image ops times bd-mrgf]
   (if (zero? times)
     image
     (transform-and-merge (bd-mrgf image (reduce (fn [im f]
                                                   (f im)) image ops))
                          ops (dec times) bd-mrgf))))

(defn replicate-image
  [image r c]
  (mapv (fn [r]
          (vec (apply concat (concat [r] (repeat c r)))))
        (vec (apply concat (concat [image] (repeat r image))))))

(defn filter-image
  [keep bg image]
  (transpose
    (apply mapv (fn [& col]
                  (mapv (fn [cv]
                          (if (= cv keep)
                            keep
                            bg)) col)) image)))

(defn func-space
  [func space]
  (map (fn [args]
         (apply partial func args)) space))

(defn color-func-space
  [func]
  (vec (func-space func (map #(vector %)
                        (range 10)))))

(defn funcs-for-ns
  [ns ns-func-space]
  (let [all (ns-publics ns)
        one-arg (map #(% all) (clojure.set/difference (set (keys all))
                                                      (set (keys ns-func-space))))]
    (filterv (fn [sym]
              (not (clojure.string/includes? (str sym) "func-space")))
            (concat one-arg (apply concat (vals ns-func-space))))))