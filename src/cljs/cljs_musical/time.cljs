(ns cljs-musical.time
  (:require [schema.core :as s
             :include-macros true
             ]))

;; http://stackoverflow.com/questions/6233927/microsecond-timing-in-javascript
(defn now [] (js/window.performance.now))

;; number of ticks per beat.
;; this is the smallest division of time we care about
(def resolution 256)

(defn bpm->millis [beats-per-minute] (/ 60000 beats-per-minute))

(defn millis-per-tick [bpm] (/ (bpm->millis bpm) resolution))

;; a map of note-value to relative duration {:1 1 :2 0.5 :4 0.25 ...}
;; :1 = whole note, :2 = half note, etc.
;; :1. = dottef whole note, :2. = dotted half note, etc. depending on resolution
(def note-value->duration (let [divisors (take-while (partial >= resolution)
                                                     (iterate (partial * 2) 1))
                                keys (map #(keyword (str %)) divisors)
                                multipliers (map #(/ 1 %) divisors)
                                dotted-divisors (take-while (partial > resolution) divisors)
                                dotted-keys (map #(keyword (str % ".")) dotted-divisors)
                                dotted-multipliers (map #(* 1.5 (/ 1 %)) dotted-divisors)]
                            (conj (zipmap keys multipliers)
                                  (zipmap dotted-keys dotted-multipliers))))

;; TODO: handle compound values: duration = 112 = :4. + :16
(def duration->note-value (clojure.set/map-invert note-value->duration))

;; https://en.wikipedia.org/wiki/Tuplet#Triplets
(defn triplet-duration [note-value] (* (/ 2 3) (note-value note-value->duration)))

(s/defrecord TimeSignature
  [beats-per-measure :- s/Int
   beat-unit :- s/Keyword])

(defn note-value->ticks [note-value] (* resolution
                                        (note-value note-value->duration)))

(defn ticks->note-value [ticks] (duration->note-value (/ ticks resolution)))

(defn ticks-per-measure [time-sig]
  (* (:beats-per-measure time-sig)
     (note-value->ticks (:beat-unit time-sig))))

;; crops a phrase to include only events that are played within the measure
;; events which start before the measure or end after the start of the next will have their
;; duration reduced to fit and assigned a flag to indicate a tie
(defn crop-phrase-to-measure [start-tick time-sig phrase]
  (let [event-start-ticks (->> (:events phrase)
                               (map :note-value)
                               (map note-value->ticks)
                               (reductions + (:start phrase)))
        next-measure-start-tick (+ start-tick (ticks-per-measure time-sig))
        event-end (fn [event] (+ (:start event)
                                 (note-value->ticks (:note-value event))))
        precedes-measure? (fn [event] (<= (event-end event) start-tick))
        starts-before-measure? (fn [event] (< (:start event) start-tick))
        starts-in-measure? (fn [event] (< (:start event) next-measure-start-tick))
        ends-after-measure? (fn [event] (> (event-end event) next-measure-start-tick))
        ]
    (->> (:events phrase)
         (map (fn [start-tick event] (assoc event :start start-tick)) event-start-ticks)
         (drop-while precedes-measure?)
         (take-while starts-in-measure?)
         (map (fn [event]
                (cond
                  (starts-before-measure? event) (assoc event
                                                   :tie true
                                                   :note-value (ticks->note-value
                                                                 (- start-tick
                                                                    (:start event))))

                  (ends-after-measure? event) (assoc event
                                                :tie true
                                                :note-value (ticks->note-value
                                                              (- next-measure-start-tick
                                                                 (:start event))))
                  :else event))))))