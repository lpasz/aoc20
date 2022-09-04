(ns advent-of-code-2020.day-15.rambunctious-recitation)

(defn turn-say [prev-ts]
  (if (<= 2 (count prev-ts))
    (- (last prev-ts) (first prev-ts))
    0))

(defn turn-say-ts [say-ts now-t]
  (if-let [say-t (last say-ts)] [say-t now-t] [now-t]))

(defn take-turn [acc start-nums [prev prev-ts] now-t stop-at-t]
  (cond (> now-t stop-at-t) prev
        (empty? start-nums) (let [say (turn-say prev-ts)
                                  say-ts (turn-say-ts (acc say) now-t)]
                              (recur (assoc acc say say-ts) [] [say say-ts] (inc now-t) stop-at-t))
        :else (let [say (first start-nums)]
                (recur (assoc acc say [now-t]) (rest start-nums) [say [now-t]] (inc now-t) stop-at-t))))

(defn will-be-said-in-nth [star-nums nth] (take-turn {} star-nums [] 1 nth))

(def ex [0 3 6]) ;; 436
(def inp [12 1 16 3 11 0]) ;; 1696

;;part 1
(will-be-said-in-nth ex 2020)
(will-be-said-in-nth inp 2020)

;; part 2
(will-be-said-in-nth ex 30000000) ;; 175594
(will-be-said-in-nth inp 30000000) ;; 37385
