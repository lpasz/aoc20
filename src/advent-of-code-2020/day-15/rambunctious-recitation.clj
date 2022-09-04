(ns advent-of-code-2020.day-15.rambunctious-recitation
  (:require [clojure.string :as str]))

(defn take-turn [acc start-nums prev now-t stop-at-t]
  (cond
    (> now-t stop-at-t) prev
    (empty? start-nums) (let [prev-ts (acc prev)
                              [t1 t2] (take-last 2 prev-ts)
                              [acc say] (if (<= 2 (count prev-ts))
                                          [(assoc acc prev [t1 t2]) (- t2 t1)]
                                          [acc 0])]
                          (recur (assoc acc say (conj (or (acc say) []) now-t))
                                 []
                                 say
                                 (inc now-t)
                                 stop-at-t))
    :else (let [say (first start-nums)]
            (recur (assoc acc say [now-t])
                   (rest start-nums)
                   say
                   (inc now-t)
                   stop-at-t))))

(defn will-be-said-in-nth [star-nums nth]
  (take-turn (sorted-map) star-nums nil 1 nth))

(def ex0 [0 3 6]) ;; 436
(def ex1 [1 3 2]) ;; 1
(def ex2 [2 1 3]) ;; 10
(def ex3 [1 2 3]) ;; 27
(def ex4 [2 3 1]) ;; 78
(def ex5 [3 2 1]) ;; 438
(def ex6 [3 1 2]) ;; 1836
(def inp [12 1 16 3 11 0]) ;; 1696

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

(will-be-said-in-nth ex0 30000000) ;; 175594
(will-be-said-in-nth inp 30000000) ;; 37385
