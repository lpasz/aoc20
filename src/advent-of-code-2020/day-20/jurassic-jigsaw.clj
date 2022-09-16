(ns advent-of-code-2020.day-20.jurassic-jigsaw
  (:require [clojure.string :as s]))

(def ex1 (slurp "./src/advent-of-code-2020/day-20/ex1.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-20/inp.txt"))

(defn transpose [m]
  (apply map vector m))

(defn tiles [text]
  (->> (s/split text #"\n\n")
       (map (fn [tile]
              (let [[tile-num & tile-info] (s/split-lines tile)
                    tile-id (Integer/parseInt (re-find #"\d+" tile-num))
                    tile-info (map seq tile-info)
                    tile-info-t (transpose tile-info)
                    borders (let [borders [(first tile-info)
                                           (last tile-info)
                                           (first tile-info-t)
                                           (last tile-info-t)]]
                              (into #{} (concat (map seq borders) (map reverse borders))))]
                [tile-id borders])))
       (into {})))

(defn product-of-edges-ids [text]
  (let [tiles (tiles text)]
    (->> tiles
         (map (fn [[tile-id1 borders1]]
                [tile-id1
                 (->> borders1
                      (filter (fn [border1] (some (fn [[tile-id2 borders2]]
                                                    (if-not (= tile-id1 tile-id2)
                                                      (borders2 border1))) tiles)))
                      (count))]))
         (filter #(= 4 (second %)))
         (reduce #(* %1 (first %2)) 1))))

(product-of-edges-ids inp)
(product-of-edges-ids ex1)
