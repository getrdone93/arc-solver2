(ns arc-solver2.core
  (:gen-class)
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

(defn make-image
  ([r c] (make-image r c 0))
  ([r c v] (vec (take r (repeat (vec (take c (repeat v))))))))

(defn cartesian-product
  [m n]
  (vec (apply concat (map (fn [mi]
                          (mapv (fn [ni]
                                  [mi ni]) (range n))) (range m)))))

(defn fractal
  [image]
  (let [rep-img (mapv (fn [r]
                        (mapv (fn [x] (if (zero? x)
                                        (make-image (count image) (count (first image)))
                                        image)) r)) image)]
    (mapv (fn [[r sr]]
            (vec (apply concat (map (fn [row]
                                      (row sr)) (rep-img r)))))
          (cartesian-product (count rep-img) (count rep-img)))))

(defn solve-problem
  [input sol-func]
  (reduce (fn [iv k]
            (assoc iv k (mapv (fn [{i "input"}]
                                {"input" i "output" (sol-func i)})
                              (input k)))) {} (keys input)))

(def problem-solution {"007bbfb7.json" fractal})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
