(ns advent-of-code-2018.day2.day2
  (:require [devcards.core :refer-macros [defcard-rg]]
            [advent-of-code-2018.day2.input :as input]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn split [id-str]
  (str/split id-str ""))

(defn analyze-ids [ids]
  (map #(->> ids
             (group-by identity)
             (map (comp count second))
             set)
       ids))

(defn checksum-parts [analyzed-ids]
  (reduce (fn [{:keys [two three]} analyzed-id]
            {:two (if (contains? analyzed-id 2) (inc two) two)
             :three (if (contains? analyzed-id 3) (inc three) three)}
            )
          {}
          analyzed-ids))

(defn checksum [checksum-parts]
  (let [{:keys [two three]} checksum-parts]
    (* two three)))

(defn day2-part1-solution [input]
  (->> (str/split-lines input)
       (map split)
       analyze-ids
       checksum-parts
       checksum))

(defn diff [id1 id2]
  (map (fn [a b] (when (= a b) a)) id1 id2))

(defn nil-count [id]
  (count (filter nil? id)))

(defn find-closest* [needle haystack]
  (let [[first & rest] haystack]
    (when first
      (let [d (diff needle first)]
        (if (= 1 (nil-count d))
          (remove nil? d)
          (recur needle rest))))))

(defn find-closest [ids]
  (let [[first & rest] ids]
    (when first
      (if-let [closest (find-closest* first rest)]
        closest
        (recur rest)))))

(defn day2-part2-solution [input]
  (->> (str/split-lines input)
       (map split)
       find-closest
       str/join))

(defcard-rg day2-part1
  [:<>
   [:h1 "Day 2, part 1"]
   [:p "Result: " (day2-part1-solution input/input)]])

(defcard-rg day2-part2
  [:<>
   [:h1 "Day 2, part 2"]
   [:p "Result: " (day2-part2-solution input/input)]])
