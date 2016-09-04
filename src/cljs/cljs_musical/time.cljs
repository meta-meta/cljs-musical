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
(def note-values (let [divisors (take-while (partial >= resolution)
                                            (iterate (partial * 2) 1))
                       keys (map #(keyword (str %)) divisors)
                       multipliers (map #(/ 1 %) divisors)]
                   (zipmap keys multipliers)))

(def duration->note-values (zipmap (vals note-values) (keys note-values)))

;; https://en.wikipedia.org/wiki/Tuplet#Triplets
(defn triplet-duration [note-value] (* (/ 2 3) (note-value note-values)))

(s/defrecord TimeSignature
  [beats-per-measure :- s/Int
   beat-unit :- s/Keyword])

(defn note-value->ticks [note-value]
  (* resolution
     (note-value note-values)))

(defn ticks-per-measure [time-sig]
  (* (:beats-per-measure time-sig)
     (note-value->ticks (:beat-unit time-sig))))

;; crops a phrase to include only notes and partial notes which should be drawn in the measure
(defn crop-phrase-to-measure [start-tick time-sig phrase]
  (let [event-start-ticks (->> (:events phrase)
                               (map :note-value)
                               (map note-value->ticks)
                               (reductions + (:start phrase)))
        start-tick-to-event (zipmap event-start-ticks (:events phrase))
        left-cropped-tick-to-events (drop-while #(<= (+ (key %)
                                                        (note-value->ticks (:note-value (val %))))
                                                     start-tick)
                                                start-tick-to-event)
        next-measure-start-tick (+ start-tick (ticks-per-measure time-sig))
        cropped-tick-to-events (take-while #(< (key %)
                                               next-measure-start-tick)
                                           left-cropped-tick-to-events)
        ]
    cropped-tick-to-events)
  )