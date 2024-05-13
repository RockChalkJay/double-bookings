(use '[clojure.test :only [is]])

(defn meetings-overlap?
  [[mtg1 mtg2]]
  (and (>= (:start mtg2) (:start mtg1))
       (< (:start mtg2) (:end mtg1))))

(defn overlapping
  "Takes a sequence of meeting maps that include start
  and end times in epoch and determines which
  meetings are double booked. The double books are
  represented by a pair of meeting maps.
  Here is a sample input:

  [{:start 100 :end 300}
   {:start 100 :end 150}
   {:start 500 :end 1000}]

   which results in:
   
   [[{:start 100, :end 150} {:start 100, :end 300}]]"
  [meetings]
  (loop [sorted-mtgs (sort-by
                       (juxt :start :end) meetings)
         results []]
    (if (> 1 (count sorted-mtgs))
      results
      (let [mtg-pairs (map (fn [x]
                             [(first sorted-mtgs) x]) (rest sorted-mtgs))
            overlaps (filter meetings-overlap? mtg-pairs)]
        (recur (rest sorted-mtgs) (apply conj results overlaps))))))


;;; TESTS ;;;

(def schedule1 [{:start 100 :end 300}
                {:start 100 :end 150}
                {:start 149 :end 300}
                {:start 500 :end 1000}])

(is (= (overlapping schedule1) [[{:start 100, :end 150} {:start 100, :end 300}]
                                [{:start 100, :end 150} {:start 149, :end 300}]
                                [{:start 100, :end 300} {:start 149, :end 300}]]))

(def schedule2 [{:start 500 :end 1000}])
(is (= (overlapping schedule2) []))

(def schedule3 [{:start 1000 :end 1500} {:start 500 :end 1000}])
(is (= (overlapping schedule3) []))

(def schedule4 [{:start 1000 :end 1500} {:start 1000 :end 1500}])
(is (= (overlapping schedule4) [[{:start 1000 :end 1500} {:start 1000 :end 1500}]]))

(def schedule5 [{:start 1000 :end 1500} {:start 1001 :end 1200}])
(is (= (overlapping schedule5) [[{:start 1000 :end 1500} {:start 1001 :end 1200}]]))

(is (overlapping nil) [])
