(ns arc-solver2.core
  (:gen-class)
  (:require [arc-solver2.image-transforms :as itf])
  (:require [arc-solver2.image-utils :as iu])
  (:require [cheshire.core :as chesh-core]))

(defn problem-path
  [problem]
  (str "/home/tanderson/git/ARC/data/training/" problem))

(defn out-path
  [file-name]
  (str "/home/tanderson/git/arc-solver2/output/" file-name))

(defn read-problem
  [file-path]
  (chesh-core/parse-string (slurp file-path)))

(defn write-solution
  [out-path structure]
  (spit out-path (chesh-core/generate-string structure)))

(defn solve-problem
  [input sol-func]
  (reduce (fn [iv k]
            (assoc iv k (mapv (fn [{i "input"}]
                                {"input" i "output" (sol-func i)})
                              (input k)))) {} (keys input)))

;;image funcs
(defn fractal
  [image]
  (let [rep-img (mapv (fn [r]
                        (mapv (fn [x] (if (zero? x)
                                        (itf/make-image (count image) (count (first image)))
                                        image)) r)) image)]
    (mapv (fn [[r sr]]
            (vec (apply concat (map (fn [row]
                                      (row sr)) (rep-img r)))))
          (itf/cartesian-product (range (count rep-img)) (range (count rep-img))))))

(defn repeat-half
  [image]
  (mapv (fn [r]
          (mapv #(if (zero? %)
                   0
                   (inc %)) r)) (vec (drop-last (/ (count image) 2)
                                                (itf/replicate-image image 1 0)))))

(defn half-intersection
  [image]
  (let [[lh rh] (itf/vert-image-halves image)]
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
                                               (itf/transpose image))))
        ;;y,g,r,b
        cols {0 4 1 3 2 2 3 1}]
    (apply itf/color-cols (concat [image]
                              [(vec (map-indexed (fn [i [ri r fc]]
                                                   (let [inds (iu/indicies-of r 5)]
                                                     [ri [(first inds) (inc (last inds))] (cols i)]))
                                                 crs))]))))

(defn crop
  [image]
  (let [[[rb re] [cb ce]] (iu/shape-coords (iu/non-bg-locs image 0))]
    (itf/slice-image image [rb (inc re)] [cb (inc ce)])))

(defn smash-cols
  [image]
  (itf/transpose (mapv #(iu/group-colors-by-row % 0 0) (itf/transpose image))))

(def problem-solution {"007bbfb7.json" fractal
                       "017c7c7b.json" repeat-half ;;mysterious no reverse on first train example
                       "0520fde7.json" half-intersection
                       "08ed6ac7.json" color-bars
                       "1cf80156.json" crop
                       "1e0a9b12.json" smash-cols})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
