(ns arc-solver2.collection-transforms
  (:require [arc-solver2.image-utils :as iu]))

(defn split-along
  [val image]
  (mapv (fn [img]
          (mapv #(vec %) img)) (filter
                                 (fn [sub-img]
                                   (not= (-> sub-img first first) val))
                                 (apply concat (map (fn [grp-rows]
                                                      (apply map (fn [& args]
                                                                   (vec args)) grp-rows))
                                                    (partition-by #(= (count %) 1)
                                                                  (map (fn [row]
                                                                         (partition-by #(= % val) row)) image)))))))

(defn layers-by-color
  [bg image]
  (mapv (fn [keep]
          (iu/filter-image #{keep} bg image)) (iu/colors image bg)))

(defn split-horizontal
  [image]
  (let [end (count image)
        end-col (-> image first count)
        half (quot end 2)]
    (reduce (fn [agg [rs cs]]
              (conj agg (iu/slice-image image rs cs))) []
            [[[0 half] [0 end-col]]
             [[half end] [0 end-col]]])))

(defn split-vertical
  [image]
  (let [end (count image)
        end-col (-> image first count)
        half (quot (-> image first count) 2)]
    (reduce (fn [agg [rs cs]]
              (conj agg (iu/slice-image image rs cs))) []
            [[[0 end] [0 half]]
             [[0 end] [half end-col]]])))

