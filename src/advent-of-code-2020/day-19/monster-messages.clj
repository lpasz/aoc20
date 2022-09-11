(ns advent-of-code-2020.day-19.monster-messages
  (:require [clojure.string :as s]))

(def inp (slurp "./src/advent-of-code-2020/day-19/inp.txt"))
(def ex1 (slurp "./src/advent-of-code-2020/day-19/ex1.txt"))

(def ex1-messages (second (s/split ex1 #"\n\n")))
(def ex1-rules (first (s/split ex1 #"\n\n")))

(defn remap-val [val]
  (if (re-find #"[a-zA-Z]+" val)
    (first (read-string val))
    (->> (s/split val #"\|")
         (map #(read-string (str "[" % "]")))
         (into []))))



(defn message-validator [text]
  (->> (s/split-lines text)
       (map #(s/split % #": "))
       (map (fn [[key val]] [(read-string key) (remap-val val)]))
       (into (sorted-map))))

(def messages (s/split-lines ex1-messages))
(def rules (message-validator ex1-rules))

(defn cartesian-prod [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian-prod (rest colls))
          x (first colls)]
      (cons x more))))

(defn insp [n]
  (do (clojure.pprint/pprint n)
      n))

(defn all-paths [start-node path]
  (let [nexts (path start-node)]
    (if (char? nexts)
      nexts
      (->> nexts
           (map (fn [next]
                  (->> next
                       (map #(all-paths % path))
                       ((fn [ns] 
                          (if (every? char? ns)
                            (apply str ns)
                            (map #(apply str %) (cartesian-prod ns))))))))))))

;; 4 -> [\a]
;; 5 -> [\b]
(all-paths 4 rules)
(all-paths 5 rules)
;; 2 -> ((\a \a) (\b \b))
(all-paths 2 rules)
(all-paths 3 rules)
;; 1 ->
(all-paths 1 {0 [[4 1 5]],
              1 [[2 2] [3 3]],
              2 [[4 4] [5 5]],
              3 [[4 5] [5 4]],
              4 \a,
              5 \b})

(all-paths 0 rules)


