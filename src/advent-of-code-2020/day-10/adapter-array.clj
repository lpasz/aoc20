(ns advent-of-code-2020.day-10.adapter-array
  (:require [clojure.string :as s]))

(def ex (slurp "./src/advent-of-code-2020/day-10/day-10-ex.txt"))
(def ex2 (slurp "./src/advent-of-code-2020/day-10/day-10-ex2.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-10/day-10.txt"))

(defn to-int-seq [text] (->> (s/split text #"\n") (map read-string)))

(def diff 3)

(defn jolt-source [source adapters]
  (conj adapters source))

(defn jolt-charge-receiver [adapters]
  (conj (vec adapters) (+ diff (apply max adapters))))

(defn jolt-jumps [source-jolt adapter-text]
  (->> (to-int-seq adapter-text)
       (jolt-source source-jolt)
       (jolt-charge-receiver)
       (sort)
       (partition 2 1)
       (map (fn [[x y]] (- y x)))
       (frequencies)))

(jolt-jumps 0 ex)
(jolt-jumps 0 ex2)
(let [r (jolt-jumps 0 inp)]
  (* (r 1) (r 3)))

(defn jolt-all-jumps [source-jolt adapter-text]
  (->> (to-int-seq adapter-text)
       (jolt-source source-jolt)
       (jolt-charge-receiver)
       (sort)))

(let [s (jolt-all-jumps 0 ex)]
  (->> (for [s1 s
             s2 s
             :when (and (< s1 s2) (<= (- s2 s1) 3))]
         [s1 s2])
       (reduce (fn [acc [k v]]
                 (if (acc k)
                   (update acc k conj v)
                   (assoc acc k [v])))
               (sorted-map))))


(def m (sort-by first (map (fn [[k v]] [k v]) {0 [1], 1 [4], 4 [5 6 7], 5 [6 7], 6 [7], 7 [10], 10 [11 12], 11 [12], 12 [15], 15 [16], 16 [19], 19 [22]})))
m
(defn func [next-n nodes]
  [0] [1] [4] [5 6 7])

(reduce (fn [acc [k vs]]
          ;; (mapcat (fn [a]
          ;;           (if (= (last a) k)
          ;;             (map #(conj a %) vs)
          ;;             a)) acc))
          (reduce (fn [acc2 a]
                    (if (= (last a) k)
                      (into [] (concat acc2 (map #(conj a %) vs)))
                      (conj acc2 a))) [] acc)) 
        #{[0]} m)



