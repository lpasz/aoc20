(ns advent-of-code-2020.day-09.encoding-error
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def ex (slurp "./src/advent-of-code-2020/day-09/day-09-ex.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-09/day-09.txt"))

(defn to-int-seq [text] (map read-string (str/split text #"\n")))

(to-int-seq ex)


(defn preamble? [val prevs]
  (some true? (for [l1 prevs
                    l2 prevs
                    :when (not= l1 l2)]
                (= val (+ l1 l2)))))

(defn not-preamble [coll n]
  (->> (keep-indexed (fn [i v] [i v]) coll)
       (filter (fn [[i val]]
                 (let [prevs-n (->> coll (drop (- i n)) (take n))]
                   (if (and (> i n) (not (preamble? val prevs-n)))
                     val))))
       first))


(= [14 127] (-> ex to-int-seq (not-preamble 5)))
(second (-> inp to-int-seq (not-preamble 25)))