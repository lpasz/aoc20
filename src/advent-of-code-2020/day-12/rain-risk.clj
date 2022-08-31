(ns advent-of-code-2020.day-12.rain-risk
  (:require [clojure.string :as s]))

(def ex (slurp "./src/advent-of-code-2020/day-12/day-12-ex.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-12/day-12.txt"))

;; :N means to move north by the given value.
;; :S means to move south by the given value.
;; :E means to move east by the given value.
;; :W means to move west by the given value.
;; :L means to turn left the given number of degrees.
;; :R means to turn right the given number of degrees.
;; :F means to move forward by the given value in the direction the ship is currently facing.

(def dir {:F :facing
          :R :right
          :L :left
          :E :east
          :N :north
          :S :south
          :W :west})

(defn to-inst [text]
  (->> (s/split text #"\n")
       (map #(let [[_ action mag] (re-find #"([A-Z]+)(\d+)" %)]
               [((keyword action) dir) (Integer/parseInt mag)]))))

(quot 180 90)

(def directions [:east :south :west :north])

(defn rotate [curr dir deg]
  (nth (cycle directions)
       (+ ((if (= dir :right) + -) (rem (quot (+ 360 deg) 90) 4))
          ({:left 4 :right 0} dir)
          (.indexOf directions curr))))

(to-inst ex)

(defn where [{:keys [east west north south]}]
  (+ (abs (- north south)) (abs (- east west))))

(defn  manhattan-distance [text]
  (->> (to-inst text)
       (reduce (fn [{:keys [facing] :as acc} [action mag]]
                     (cond
                       (= action :facing) (update acc facing + mag)
                       (#{:right :left} action) (update acc :facing (fn [_] (rotate facing action mag)))
                       :else (update acc action + mag)))
                   {:facing :east, :east 0, :north 0, :south 0, :west 0})
       (where)))

(manhattan-distance ex) ;; 25
(manhattan-distance inp);; 1319
