(ns advent-of-code-2020.day-09.encoding-error
  (:require [clojure.string :as str]))

(def ex (slurp "./src/advent-of-code-2020/day-09/day-09-ex.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-09/day-09.txt"))

(defn to-int-seq [text] (map read-string (str/split text #"\n")))

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

(defn drop-until-sum-lower [coll val]
  (if (<= (apply + coll) val)
    (into [] coll)
    (recur (rest coll) val)))

(defn get-sum-range [coll sum]
  (reduce (fn [[_ prevs] curr]
            (let [prevs-n (drop-until-sum-lower prevs sum)
                  prevs-sum (apply + prevs-n)]
              (if (= prevs-sum sum)
                [:yep prevs-n]
                [:nop (conj prevs-n curr)]))) [] coll))

(defn encryption-weakness [coll sum]
  (let [[_ range] (get-sum-range coll sum)]
    (+ (apply max range) (apply min range))))

(= 62 (-> ex to-int-seq (encryption-weakness 127)))
(= 2942387 (-> inp to-int-seq (encryption-weakness 22406676)))