(ns advent-of-code-2020.day-03.toboggan-trajectory
  (:require [clojure.string :as str]))

(def example
  (slurp "src/advent-of-code-2020/day-03/day-03-ex.txt"))

(def input
  (slurp "src/advent-of-code-2020/day-03/day-03.txt"))



(defn to-usable-input [text]
  (->> (str/split-lines text)
       (map #(into [] (seq %)))
       (into [])))

(def toboggan-matrix-example (to-usable-input example))
(def toboggan-matrix-input (to-usable-input input))

toboggan-matrix-example
toboggan-matrix-input

(defn toboggan-x [toboggan-matrix] (count (toboggan-matrix 0)))

(defn get-reposition [x y toboggan-matrix] 
  (-> (get toboggan-matrix y)
      (get (rem x (toboggan-x toboggan-matrix)))))

(defn count-trees-on-way [trees [x y] [x+ y+] toboggan-matrix]
  (let [position (get-reposition x y toboggan-matrix)
        is-tree (= position \#)
        next-x (+ x x+) 
        next-y (+ y y+)]
    (clojure.pprint/pprint [:x x
                            :y y
                            :position position
                            :next-x next-x
                            :next-y next-y
                            :is-tree is-tree])
    (cond
      (nil? position) trees
      is-tree (recur (inc trees) [next-x next-y] [x+ y+] toboggan-matrix)
      :else (recur trees [next-x next-y] [x+ y+] toboggan-matrix))))

(count-trees-on-way 0 [0 0] [3 1] toboggan-matrix-input)

(count-trees-on-way 0 [0 0] [3 1] toboggan-matrix-example)


(->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
     (map #(count-trees-on-way 0 [0 0] % toboggan-matrix-example))
     (reduce *))

(->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
     (map #(count-trees-on-way 0 [0 0] % toboggan-matrix-input))
     (reduce *))




