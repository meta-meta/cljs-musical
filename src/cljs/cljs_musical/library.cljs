(ns cljs-musical.library
  (:require [schema.core :as s
             :include-macros true
             ]))

;; NoteSequence is a vector of MIDI note numbers
(s/defrecord NoteSequence
             [name :- s/Str
              seq :- [s/Int]])

;; IntervalSequence is a vector of intervals
(s/defrecord IntervalSequence
             [name :- s/Str
              seq :- [s/Int]])

(defn all-positive? [vec]
  (reduce (fn [acc val] (and acc (> val 0))) seq))

(defn sum-vec [vec] (reduce + vec))

;; Scale is an ascending interval sequence guaranteed to span exactly an octave
(s/defrecord Scale
             [name :- s/Str
              seq :- (s/pred (fn [seq]
                               (and
                                 (= 12 (sum-vec seq))
                                 (all-positive? seq))
                               ))])


;; PITCH CLASS   PITCH CLASS   PITCH CLASS   PITCH CLASS   PITCH CLASS   PITCH CLASS   PITCH CLASS   PITCH CLASS


(defn compare-pitch-class [a b]
  (let [a (name a)
        b (name b)
        flat? #(and (= 2 (count %))
                    (clojure.string/ends-with? % "b"))
        root #(subs % 0 1)]
    (if (= (root a) (root b))
      (if (flat? a) -1 1)
      (compare a b))))


(def pitch-classes (sorted-set-by compare-pitch-class :c :db :d :e :eb :f :f# :g :ab :a :bb :b))

(def pc-cycle-up (cycle pitch-classes))

(defn pitch-class? [pc] (contains? (set pitch-classes) pc))

(def PitchClass (s/pred #(contains? (set pitch-classes) %) "contained in pitch-classes"))



;; KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY


;; Key is a set of pitch-classes and a tonic
;TODO: pitch-classes must contain tonic
;TODO: will there ever be a case where first pitchclass is not tonic?
(s/defrecord Key
             [name :- s/Str
              pitch-classes :- [PitchClass]
              tonic :- PitchClass])

(def diatonic-scale (Scale. :diatonic [2 2 1 2 2 2 1]))

(defn scale-to-note-seq [scale starting-note]
  (->> (range (count (:seq scale)))
       (map #(reduce + starting-note (take % (:seq scale))))))

;(defn key-from-scale [scale tonic]
;  (Key.
;    (:name scale)
;    (->> (scale-to-note-seq scale tonic)
;         (map #(mod % 12)))
;    tonic
;    )
;  )

(defn scale->key-name [scale tonic]
  (keyword (str (name tonic) "-" (name (:name scale)))))

(defn scale->key-iter [scale pitches]
  (cons (first pitches)
        (if-not (empty? scale)
          (scale->key-iter (drop 1 scale)
                           (drop (first scale) pitches)))))

(defn scale->key [scale tonic]
  (Key. (scale->key-name scale tonic)
        (scale->key-iter (:seq scale)
                         (drop-while #(not= tonic %) pc-cycle-up))
        tonic))





(def library {
              :names                           {
                                                :chromatic  "Chromatic"
                                                :diatonic   "Diatonic"
                                                :pentatonic "Pentatonic"
                                                :whole-tone "Whole Tone"
                                                :octatonic  "Octatonic"
                                                }

              :scales                          {
                                                :chromatic  (Scale. :chromatic (repeat 12 1))
                                                :diatonic   diatonic-scale
                                                :pentatonic (Scale. :pentatonic [3 2 3 2 2])
                                                :whole-tone (Scale. :whole-tone [2 2 2 2 2 2])
                                                :octatonic  (Scale. :octatonic [1 2 1 2 1 2 1 2])
                                                }

              :keys                            (zipmap (map (partial scale->key-name diatonic-scale)
                                                            pitch-classes)
                                                       (map (partial scale->key diatonic-scale)
                                                            pitch-classes))

              :pitch-classes                   pitch-classes
              :pc-cycle-up                     pc-cycle-up
              :pc-cycle-dn                     (cycle (reverse pitch-classes))

              :notes-all                       (set (range 0 128))
              :notes-ewi                       (set (range 34 100))

              ;TODO BONUS! write comparator for bright-to-dark
              :mode-names-diatonic-order       [:ionian :dorian :phrygian :lydian :mixolydian :aeolian :locrian]
              :mode-names-bright-to-dark-order [:lydian :ionian :mixolydian :dorian :aeolian :phrygian :locrian]
              })