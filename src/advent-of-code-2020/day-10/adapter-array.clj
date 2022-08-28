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
  (* (r 1) (r 3))) ;; 1914

(defn all-jumps-available [jumps]
  (let [s jumps]
    (->> (for [s1 s
               s2 s
               :when (and (< s1 s2) (<= (- s2 s1) 3))]
           [s1 s2])
         (reduce (fn [acc [k v]]
                   (if (acc k)
                     (update acc k conj v)
                     (assoc acc k [v])))
                 (sorted-map)))))

(defn paths-start-finish [path-map]
  (reduce (fn [acc [key vals]]
            (-> (reduce (fn [acc v]
                          (if (acc v)
                            (update acc v #(+ % (acc key)))
                            (assoc acc v (acc key 1))))
                        acc vals)
                (dissoc key)))
          {0 1}
          path-map))

(defn count-all-available-paths [source-jolt adapter-text]
  (->> (to-int-seq adapter-text)
       (jolt-source source-jolt)
       (jolt-charge-receiver)
       (sort)
       (all-jumps-available)
       (paths-start-finish)
       (vals)
       (first)))

(count-all-available-paths 0 ex) ;; 8
(count-all-available-paths 0 ex2) ;; 19208
(count-all-available-paths 0 inp) ;; 9256148959232