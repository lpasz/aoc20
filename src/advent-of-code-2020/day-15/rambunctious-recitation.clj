(ns advent-of-code-2020.day-15.rambunctious-recitation
  (:require [clojure.string :as str]))

(defn incr [value] (if value
                     (inc value)
                     1))

(defn time-between [acc k]
  (let [[[t1 _] [t2 _]] (->> (:spoke acc)
                             (into (sorted-map))
                             (filter (fn [[_ v]] (= v k)))
                             (take-last 2))]
    (- t2 t1)))

(time-between {:spoke {1 0, 4 0, 6 3, 3 6, 2 3, 9 0, 5 3}} 3)


(defn turn [start-nums acc now stop-at]
  (cond
    (> now stop-at) (->> (:spoke acc) (into (sorted-map)) (last) (second))
    (empty? start-nums) (let [prev-num-spoken (get-in acc [:spoke (dec now)])
                              prev-num-spoken-times (get-in acc [:count prev-num-spoken])
                              num-to-say (if (= prev-num-spoken-times 1)
                                           0
                                           (time-between acc prev-num-spoken))]
                          (recur []
                                 (-> acc
                                     (assoc-in [:spoke now] num-to-say)
                                     (update-in [:count num-to-say] incr))
                                 (inc now)
                                 stop-at))

    :else (let [start-num (first start-nums)
                rest-start-nums (rest start-nums)]
            (recur rest-start-nums
                   (-> acc
                       (assoc-in [:spoke now] start-num)
                       (update-in [:count start-num] incr))
                   (inc now)
                   stop-at))))

(defn will-be-said-in-nth [star-nums nth]
  (turn star-nums {:spoke (sorted-map)
                   :count (sorted-map)} 1 nth))

(def ex0 [0 3 6]) ;; 436
(def ex1 [1 3 2]) ;; 1
(def ex2 [2 1 3]) ;; 10
(def ex3 [1 2 3]) ;; 27
(def ex4 [2 3 1]) ;; 78
(def ex5 [3 2 1]) ;; 438
(def ex6 [3 1 2]) ;; 1836
(def inp [12 1 16 3 11 0])

;;part 1

(will-be-said-in-nth ex0 2020)
(will-be-said-in-nth ex1 2020)
(will-be-said-in-nth ex2 2020)
(will-be-said-in-nth ex3 2020)
(will-be-said-in-nth ex4 2020)
(will-be-said-in-nth ex5 2020)
(will-be-said-in-nth ex6 2020)
(will-be-said-in-nth inp 2020)

;; part 2
;; Given 0,3,6, the 30000000th number spoken is 175594.
;; Given 1,3,2, the 30000000th number spoken is 2578.
;; Given 2,1,3, the 30000000th number spoken is 3544142.
;; Given 1,2,3, the 30000000th number spoken is 261214.
;; Given 2,3,1, the 30000000th number spoken is 6895259.
;; Given 3,2,1, the 30000000th number spoken is 18.
;; Given 3,1,2, the 30000000th number spoken is 362.

(will-be-said-in-nth ex0 30000000)


