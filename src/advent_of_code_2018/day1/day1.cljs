(ns advent-of-code-2018.day1
  (:require [devcards.core :refer-macros [defcard-rg]]
            [clojure.string :as str]
            [advent-of-code-2018.day1.input :as input]))

(defn parse-num [num]
  (-> num
      str/trim
      (str/replace "\\+" "")
      js/Number.parseInt))

(defn parse-input [input]
  (map parse-num (str/split-lines input)))

(def parsed-input
  (parse-input input/input))

(defn calc-day1-1 [input]
  (reduce + 0 (parse-input input)))

(defn calc-day1-2 [input]
  (reduce (fn [{:keys [seen current]} change]
            (if (seen current)
              (reduced current)
              {:seen (conj seen current)
               :current (+ current change)}))
          {:seen #{}
           :current 0}
          (cycle (parse-input input))))

(defcard-rg day-1-part-1
  [:<>
   [:h1 "Day 1"]
   [:p "Result: " (calc-day1-1 parsed-input)]])


(defcard-rg day-1-part-2
  [:<>
   [:h1 "Day 2"]
   [:p "Result: " (str (calc-day1-2 parsed-input))]])



