(ns advent-of-code-2020.day-20.jurassic-jigsaw
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def ex1 (slurp "./src/advent-of-code-2020/day-20/ex1.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-20/inp.txt"))

(defn transpose [m]
  (apply map vector m))

(defn rotate-tile [m]
  (map reverse (transpose m)))

(defn all-rotations [m]
  (loop [prev m
         result #{}]
    (let [next-tile (rotate-tile prev)]
      (if-not (result next-tile)
        (recur next-tile (conj result next-tile))
        result))))

(defn flip-tile [m] (map reverse m))

(defn flip-tiles [ms]
  (->> ms
       (mapcat (fn [m] [m (flip-tile m)]))))

(defn all-variants [m]
  (->> m
       (all-rotations)
       (flip-tiles)))

(all-variants [[1 2] [3 4]])

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
                              (into #{} (concat
                                         (map seq borders)
                                         (map reverse borders))))]
                [tile-id {:borders borders :info tile-info}])))
       (into {})))

(defn tiles-possibles-edges [tiles]
  (->> tiles
       (map (fn [[tile-id1 {borders1 :borders}]]
              [tile-id1
               (->> borders1
                    (keep (fn [border1] (keep (fn [[tile-id2 {borders2 :borders}]]
                                                (if (and (not= tile-id1 tile-id2) (borders2 border1))
                                                  tile-id2)) tiles))))]))
       (map (fn [[k v]] [k (into #{} (flatten v))]))))

(defn edge-tiles [tiles]
  (->>    (tiles-possibles-edges tiles)
          (filter #(= 2 (count (second %))))
          (map first)))

(defn product-of-edges-ids [text]
  (let [tiles (tiles text)]
    (->> (edge-tiles tiles)
         (apply *))))

(product-of-edges-ids inp)
(product-of-edges-ids ex1)


(defn match? [center upper left right down]
  (if (and (or (nil? upper) (= (first center) (last upper)))
           (or (nil? left) (= (first (transpose center)) (last (transpose left))))
           (or (nil? down) (= (last center) (first down)))
           (or (nil? right) (= (last (rotate-tile center)) (first (transpose right)))))
    {:center center
     :upper upper
     :left left
     :right right
     :down down}))

(defn get-tile-id-variants [id ts]
  (some->> (ts id)
           (:info)
           (all-variants)))

(defn some-edge [tpe]
  (->> tpe
       (filter #(= 2 (count (second %))))
       (last)))

(some-edge ex1)

(defn next-left [current border-id ts]
  (first (for [border (get-tile-id-variants border-id ts)
               :when  (= (last (transpose current)) (first (transpose border)))]
           [border-id border])))


(defn make-line [line ts tpb mx]
  (if-not (= 3 (count line))
    (let [[tile-id tile-info] (first line)
          next (first (keep #(next-left tile-info % ts) (tpb tile-id)))]
      (recur (conj line next) ts tpb mx))
    line))

(defn assembly-tiles [text size]
  (let [ts (tiles text)
        tpe (into {} (tiles-possibles-edges ts))
        [s-id se-ids] (some-edge tpe)
        s-grid (:info (ts s-id))
        f-line (make-line (list [s-id s-grid]) ts tpe size)]
    (->> f-line
         (reverse)
         (map (fn [[id info]]
                (make-line (list [id (rotate-tile info)]) ts tpe size)))
         (map reverse))))


(assembly-tiles ex1 3)
(assembly-tiles inp 12)

(:info ((tiles ex1) 2971))

(all-variants [[1951 2311 3079]
               [2729 1427 2473]
               [2971 1489 1171]])