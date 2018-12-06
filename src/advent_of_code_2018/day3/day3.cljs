(ns advent-of-code-2018.day3.day3
  (:require [advent-of-code-2018.day3.input :as input]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[id left top width height] (->> line
                                        (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
                                        rest
                                        (map js/parseInt))]
    {:id id
     :left left
     :top top
     :width width
     :height height}))

(defn parsed-input [input]
  (->> input
       str/split-lines
       (map parse-line)))

(def parsed-input* (parsed-input input/input))

(def claims parsed-input*)

(def ids
  (into #{}
        (map :id claims)))

(defn concat! [a b]
  (reduce conj! a b))

(defn expand [{:keys [top left width height]}]
  (for [x (range left (+ left width))
        y (range top (+ top height))]
    [x y]))

(defn construct-loc->id [claims]
  (persistent!
   (reduce (fn [loc->id claim]
             (reduce (fn [loc->id loc]
                       (assoc! loc->id loc (conj (get loc->id loc #{}) (:id claim))))
                     loc->id
                     (expand claim)))
           (transient {})
           claims)))

(def loc->id (construct-loc->id claims))

(comment
  (count
   (construct-loc->id parsed-input*))

  (def i (parsed-input input/input))

  (+ 1 1)

  (expand parsed-input*)

  (->> [[1 1] [1 1] [3 3]]
       frequencies
       (filter (fn [[_ freq]] (< 1 freq)))
       count
       )

  (def expanded
    (->> parsed-input*
         expand
         frequencies
         (filter (fn [[_ freq]] (< 1 freq)))
         count))

  (count loc->id)

  (def result
    (->> loc->id
         (filter (fn [[_ ids]] (< 1 (count ids))))
         count)
    )

  (reduce-kv
   (fn [non-overlapping-ids _ ids-in-loc]
     (if (< 1 (count ids-in-loc))
       (set/difference non-overlapping-ids ids-in-loc)
       non-overlapping-ids))
   ids
   loc->id)


  (count expanded)
)

