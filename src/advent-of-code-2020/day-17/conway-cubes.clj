(ns advent-of-code-2020.day-17.conway-cubes
  (:require [clojure.string :as s]))

(def inp (slurp "./src/advent-of-code-2020/day-17/inp.txt"))
(def ex1 (slurp "./src/advent-of-code-2020/day-17/ex1.txt"))

(defn distance [x]
  (range (dec x) (+ 2 x)))

(distance 1)

(defn neighbors-coord [[x y z]]
  (for [i (distance x)
        j (distance y)
        k (distance z)
        :when (not= [x y z] [i j k])]
    [i j k]))

(neighbors-coord [0 0 0])

(defn str->pocket-dimention [text]
  (->> (s/split text #"\n")
       (keep-indexed (fn [y line]
                       (->> line
                            (keep-indexed (fn [x itm] (when (= \# itm) [[x y 0] true]))))))
       (reduce (fn [acc states] (->>
                                 states
                                 (reduce (fn [acc [coord state]]
                                           (assoc acc coord state)) acc)))
               {})))

(defn count-atives [coord dimention]
  (->> coord
       (neighbors-coord)
       (map dimention)
       (filter true?)
       (count)))

(defn remain-active? [coord dimention]
  (boolean (#{2 3} (count-atives coord dimention))))

(defn activate? [coord dimention]
  (boolean (#{3} (count-atives coord dimention))))

(defn new-state [coord dimention]
  (if (dimention coord)
    (remain-active? coord dimention)
    (activate? coord dimention)))


(->> (str->pocket-dimention ex1)
     (new-state [0 0 0]))

(defn new-dimention [dimention]
  (->> dimention
       (keys)
       (mapcat neighbors-coord)
       (into #{})))

(new-dimention (str->pocket-dimention ex1))

(defn do-cycle [dimention]
  (->> dimention
       (new-dimention)
       (reduce (fn [acc coord]
                 (if (new-state coord dimention)
                   (assoc acc coord true)
                   acc))
               (sorted-map))))

(defn after-n-cycles [n dimention]
  (reduce (fn [dimention _] (do-cycle dimention)) dimention (range n)))

(defn active-after-n-cycles [n text]
  (->> text
       (str->pocket-dimention)
       (after-n-cycles n)
       (filter second)
       (count)))

(active-after-n-cycles 6 ex1) ;; 112
(active-after-n-cycles 6 inp) ;; 218