(ns advent-of-code-2020.day-12.rain-risk
  (:require [clojure.string :as s]))

(def ex (slurp "./src/advent-of-code-2020/day-12/day-12-ex.txt"))
(def inp (slurp "./src/advent-of-code-2020/day-12/day-12.txt"))

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

(def directions [:east :south :west :north])

(defn rotate [curr dir deg]
  (nth (cycle directions)
       (+ ((if (= dir :right) + -) (rem (quot (+ 360 deg) 90) 4))
          ({:left 4 :right 0} dir)
          (.indexOf directions curr))))

(defn manhattan-distance [{:keys [east west north south]}]
  (+ (abs (- north south)) (abs (- east west))))

(defn  calc-position-1 [text]
  (->> (to-inst text)
       (reduce (fn [{:keys [facing] :as acc} [action mag]]
                 (cond
                   (= action :facing) (update acc facing + mag)
                   (#{:right :left} action) (update acc :facing (fn [_] (rotate facing action mag)))
                   :else (update acc action + mag)))
               {:facing :east, :east 0, :north 0, :south 0, :west 0})
       (manhattan-distance)))

(calc-position-1 ex) ;; 25
(calc-position-1 inp) ;; 1319

(defn rotate-waypoint [waypoint dir deg]
  (reduce (fn [acc [k v]] (assoc acc (rotate k dir deg) v)) {} waypoint))


(defn  calc-position-2 [text]
  (->> (to-inst text)
       (reduce (fn [acc [action mag]]
                 (cond
                   (= :facing action) (reduce
                                       (fn [acc key] (update-in acc [:ship key] #(+ % (* mag (key (:waypoint acc))))))
                                       acc
                                       directions)
                   (#{:right :left} action) (update acc :waypoint #(rotate-waypoint % action mag))
                   :else (update-in acc [:waypoint action] + mag)))

               {:waypoint {:east 10, :north 1, :south 0, :west 0}
                :ship {:east 0, :north 0, :south 0, :west 0}})
       :ship
       (manhattan-distance)))

(calc-position-2 ex) ;; 286
(calc-position-2 inp) ;; 62434


