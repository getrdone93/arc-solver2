(ns arc-solver2.image-utils)

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

