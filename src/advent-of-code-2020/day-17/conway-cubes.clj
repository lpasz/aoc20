(ns advent-of-code-2020.day-17.conway-cubes
  (:require [clojure.string :as s]))

(def inp (slurp "./src/advent-of-code-2020/day-17/inp.txt"))
(def ex1 (slurp "./src/advent-of-code-2020/day-17/ex1.txt"))

(defn distance [x]
  (range (dec x) (+ 2 x)))

(defn cartesian-prod [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian-prod (rest colls))
          x (first colls)]
      (cons x more))))

(defn neighbors-coord [coord]
  (->> coord
       (map distance)
       (cartesian-prod)
       (filter #(not= % coord))))


(defn pocket-dimention [text n-dimentions]
  (->> (s/split-lines text)
       (keep-indexed (fn [y line]
                       (->> line
                            (keep-indexed (fn [x itm] (when (= \# itm) (concat [x y] (repeat (- n-dimentions 2) 0))))))))
       (flatten)
       (partition n-dimentions)
       (set)))

(pocket-dimention ex1 4)

(defn count-atives [coord dimention]
  (->> coord
       (neighbors-coord)
       (filter dimention)
       (count)))

(defn remain-active? [coord dimention]
  (boolean (#{2 3} (count-atives coord dimention))))

(defn activate? [coord dimention]
  (boolean (#{3} (count-atives coord dimention))))

(defn new-state [coord dimention]
  (if (dimention coord)
    (when (remain-active? coord dimention) coord)
    (when (activate? coord dimention) coord)))

(defn new-dimention [dimention]
  (->> dimention
       (mapcat neighbors-coord)
       (into #{})))

(defn do-cycle [dimention]
  (->> dimention
       (new-dimention)
       (reduce (fn [acc coord]
                 (if (new-state coord dimention)
                   (conj acc coord)
                   acc))
               #{})))

(defn after-n-cycles [n dimention]
  (reduce (fn [dimention _] (do-cycle dimention)) dimention (range n)))

(defn active-after-n-cycles [n text n-dim]
  (->> (pocket-dimention text n-dim)
       (after-n-cycles n)
       (filter second)
       (count)))

;; part 1
(active-after-n-cycles 6 ex1 3) ;; 112
(active-after-n-cycles 6 inp 3) ;; 218

;; part 2
(active-after-n-cycles 6 ex1 4) ;; 848
(active-after-n-cycles 6 inp 4) ;; 1908
