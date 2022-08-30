(ns advent-of-code-2020.day-11.seating-system
  (:require [clojure.string :as s]
            [clojure.string :as string]))

(def ex (slurp "./src/advent-of-code-2020/day-11/day-11-ex.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-11/day-11.txt"))

(def free \L)
(def occupied \#)

(defn to-seats [text]
  (->> (s/split text #"\n")
       (map-indexed (fn [i vs] (map-indexed (fn [j v] [[i j] v]) vs)))
       (reduce (fn [acc xs]
                 (reduce (fn [acc [key val]]
                           (assoc acc key val)) acc xs))
               (sorted-map))))

(defn adjacent-pos-to [[y x]]
  (for [f1 [inc dec identity]
        f2 [inc dec identity]
        :let [ax (f1 x)
              ay (f2 y)]
        :when (not (and (= ax x) (= ay y)))]
    [ay ax]))

(defn no-one-nearby? [seat seats]
  (->> (adjacent-pos-to (first seat))
       (map seats)
       (some #(= \# %))
       (not)))

(defn too-many-occupied-nearby? [seat max seats]
  (->> (adjacent-pos-to (first seat))
       (map seats)
       (filter #(= \# %))
       (count)
       (<= max)))

(defn round [seats]
  (->> seats
       (map (fn [seat]
              (let [[pos kind] seat]
                (cond (and (= kind free)
                           (no-one-nearby? seat seats)) [pos occupied]
                      (and (= kind occupied)
                           (too-many-occupied-nearby? seat 4 seats)) [pos free]
                      :else seat))))
       (into (sorted-map))))

(defn stabilized-seats [seats rounder]
  (loop [curr seats]
    (let [next (rounder curr)]
      (if (= next curr)
        (count (filter #(= (second %) occupied) curr))
        (recur next)))))

(stabilized-seats (to-seats ex) round) ;; 37
(stabilized-seats (to-seats inp) round) ;; 2319

;; PART 2

(defn- first-in-dir [direction [y x] seats]
  (->> (iterate inc 1)
       (map (fn [step]
              (cond
                (= direction :up) [(- y step) x]
                (= direction :down) [(+ y step) x]
                (= direction :left) [y (- x step)]
                (= direction :right) [y (+ x step)]
                (= direction :up-left) [(- y step) (- x step)]
                (= direction :down-left) [(+ y step) (- x step)]
                (= direction :up-right) [(- y step) (+ x step)]
                (= direction :down-right) [(+ y step) (+ x step)])))
       (map #(seats %))
       (filter #(not= \. %))
       (first)))

(defn- first-in-all-dir [from seats]
  (map #(first-in-dir % from seats) [:up :down :left :right :up-left :down-left :up-right :down-right]))

(defn can-occupy-seat [[y x] seats]
  (->> seats
       (first-in-all-dir [y x])
       (every? #(or (nil? %) (= \L %)))))

(defn should-leave-seat [[y x] seats]
  (->> seats
       (first-in-all-dir [y x])
       (filter #(= % \#))
       (count)
       (<= 5)))

(defn new-round [seats]
  (->> seats
       (map (fn [seat]
              (let [[pos kind] seat]
                (cond (and (= kind free)
                           (can-occupy-seat pos seats)) [pos occupied]
                      (and (= kind occupied)
                           (should-leave-seat pos seats)) [pos free]
                      :else seat))))
       (into (sorted-map))))

(stabilized-seats (to-seats ex) new-round) ;; 26
(stabilized-seats (to-seats inp) new-round) ;; 2117
