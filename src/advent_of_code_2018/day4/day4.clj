(ns advent-of-code-2018.day4.day4
  (:require [advent-of-code-2018.day4.input :as input]
            [clojure.string :as str]))

(def action->kw
  {"falls asleep" :falls-asleep
   "wakes up" :wakes-up
   "begins shift" :begins-shift})

(defn parse-input-line
  [line]
  (let [[_ year month day hour minute unparsed-action]
        (re-matches #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)" line)

        [action guard]
        (if-let [[_ new-guard] (re-matches #"Guard #(\d+) begins shift" unparsed-action)]
          [(action->kw "begins shift") (Integer/parseInt new-guard)]
          [(action->kw unparsed-action) nil])]

    {:year (Integer/parseInt year)
     :month (Integer/parseInt month)
     :day (Integer/parseInt day)
     :hour (Integer/parseInt hour)
     :minute (Integer/parseInt minute)
     :guard guard
     :action action}))

(defn fill-guards [events]
  (:events
   (reduce (fn [acc event]
             (let [{:keys [events current-guard]} acc
                   {:keys [action guard]} event
                   resolved-guard (if (= :begins-shift action)
                                    guard
                                    current-guard)]
               {:events (conj events (assoc event :guard resolved-guard))
                :current-guard resolved-guard}))
           {:events []
            :current-guard nil}
           events)))

(defn event-str [{:keys [year month day hour minute guard action]}]
  (str/join ":" [year month day hour minute guard action]))

(def input-lines
  (-> input/input
      str/split-lines))

(def events
  (->> input-lines
       (map parse-input-line)
       (sort-by (juxt :year :month :day :hour :minute))
       fill-guards))

(defn calculate-sleep [events]
  (:sleep-minutes
   (reduce (fn [{:keys [fell-asleep sleep-minutes] :as acc} event]
             (case (:action event)
               :falls-asleep {:fell-asleep event
                              :sleep-minutes sleep-minutes}
               :wakes-up {:fell-asleep nil
                          :sleep-minutes (into sleep-minutes (range (:minute fell-asleep) (:minute event)))}
               ;; else
               acc))
           {:fell-asleep nil
            #_#_:sleep-total 0
            :sleep-minutes []}
           events)))

(def guard->sleep-minutes
  (->> events
       (group-by :guard)
       (map (fn [[guard events]] [guard (calculate-sleep events)]))))

(def sleepiest-guard
  (->> guard->sleep-minutes
       (sort-by (comp count second))
       last))

(def sleep-minutes-sorted
  (->> (frequencies (second sleepiest-guard))
       (sort-by second)))

(def guard->minute-freqs
  (map (fn [[guard minutes]] [guard (frequencies minutes)])
       guard->sleep-minutes))

(def minutes->guard-freqs
  (reduce (fn [acc [guard freqs]]\
            (reduce (fn [acc* [min freq]]
                      (update acc* min conj [guard freq]))
                    acc
                    freqs))
          {}
          guard->minute-freqs))

(def sleepiest-guard-minute
  (reduce (fn [[_ sleepiest-minute-freq _ :as max] [minute guard+freq]]
            (let [max-freq (->> guard+freq
                                (sort-by second)
                                last)]
              (if (< sleepiest-minute-freq (second max-freq))
                [(first max-freq) (second max-freq) minute]
                max)))
          [nil 0 nil]
          minutes->guard-freqs))
