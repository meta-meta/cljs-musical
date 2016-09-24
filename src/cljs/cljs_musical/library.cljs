(ns cljs-musical.library
  (:require [schema.core :as s
             :include-macros true
             ]))


;; NOTE    NOTE    NOTE    NOTE    NOTE    NOTE    NOTE    NOTE    NOTE    NOTE    NOTE

(def notes-all (into (sorted-set) (range 0 128)))

(def Note (s/pred #(contains? notes-all %) "is a valid note"))

;; NoteSequence is a vector of MIDI note numbers
(s/defrecord NoteSequence
  [name :- s/Str
   seq :- [Note]])

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


(def pitch-classes (sorted-set-by compare-pitch-class :C :Db :D :E :Eb :F :F# :G :Ab :A :Bb :B))

(def pc-cycle-up (drop 4 (cycle pitch-classes)))            ;drop 4 so we start on :c
(def pc-cycle-dn (drop 7 (cycle (reverse pitch-classes))))  ;drop 7 so we start on :c

(defn natural? [pc] (= 1 (count (name pc))))
(defn include-naturals [pc] (filter natural? pc))
(def pc-cycle-nat-up (include-naturals pc-cycle-up))
(def pc-cycle-nat-dn (include-naturals pc-cycle-dn))

(defn pitch-class? [pc] (contains? (set pitch-classes) pc))

(def PitchClass (s/pred pitch-class? "contained in pitch-classes"))

; PITCH CLASS SET       PITCH CLASS SET       PITCH CLASS SET       PITCH CLASS SET

;; PitchClassSet is a set of pitch-classes and a tonic
;; I'm totally making this up. I thought Key could be defined this way but it doesn't make sense.
;TODO: does it make sense for a pitch-class-set to contain a tonic?
(s/defrecord PitchClassSet
  [name :- s/Str
   pitch-classes :- [PitchClass]
   tonic :- PitchClass])

(def diatonic-scale (Scale. :diatonic [2 2 1 2 2 2 1]))

;TODO should interval-seq be the seq or the record wrapping a seq?
(defn interval-seq->note-seq [interval-seq starting-note]
  (reductions + starting-note interval-seq))

(defn scale->pcs-name [scale tonic]
  (keyword (str (name tonic) "-" (name (:name scale)))))

(defn scale->pcs-iter [scale pitches]
  (cons (first pitches)
        (if-not (empty? scale)
          (scale->pcs-iter (drop 1 scale)
                           (drop (first scale) pitches)))))

(defn scale->pitch-class-set [scale tonic]
  (PitchClassSet. (scale->pcs-name scale tonic)
                  (scale->pcs-iter (drop-last (:seq scale))
                                   (drop-while #(not= tonic %) pc-cycle-up))
                  tonic))


;; KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY   KEY

(defn scale->key-name [scale tonic]
  (keyword (str (name tonic) "-" (if (= :diatonic (:name scale)) :major :minor))))

(defn all-notes-in-interval-seq [seq first-note]
  (->> notes-all
       (filter
         #(contains? (set (interval-seq->note-seq seq first-note))
                     (mod % 12)))))


(def note-num->pitch-class (zipmap notes-all pc-cycle-up))
(def letter->note-num {;TODO: programatically #, b, E, B
                       :C  0
                       :C# 1
                       :Db 1
                       :D  2
                       :D# 3
                       :Eb 3
                       :E  4
                       :Fb 4
                       :F  5
                       :F# 6
                       :Gb 6
                       :G  7
                       :G# 8
                       :Ab 8
                       :A  9
                       :A# 10
                       :Bb 10
                       :B  11
                       :Cb 11
                       })

(s/defrecord Key
  [name :- s/Str
   notes :- [Note]
   tonic :- Note
   tonic-name :- s/Keyword])

(defn scale->key [scale tonic-name]                         ;tonic-name because enharmonic
  (let [tonic-num (letter->note-num tonic-name)]
    (Key. (scale->key-name scale tonic-name)
          (all-notes-in-interval-seq (:seq scale) tonic-num)
          tonic-num
          tonic-name
          ))
  )


; here we generate a default chromatic spelling for each key, prefering # or b based on how the diatonic notes are spelled. if the diatonic scale contains #, prefer #. this leaves :C ambiguous. the algorithm here ends up using b
; the pitch 7 in key of :C# should be Fx(double sharp) technically
; at some point we'll want to account for musical context (# for ascending accidentals, b for decending) but the rule is ill-defined and subjective: http://music.stackexchange.com/questions/8329/temporarily-changing-keys-which-accidentals-to-use
; more here: http://www.themusicalear.com/sharps-or-flats-how-to-spell-notes-correctly/
; https://www.wolframalpha.com/input/?i=c-sharp+major+scale
(defn spell-notes [key]
  (let [
        start-num (:tonic key)
        notes-in-key (interval-seq->note-seq (drop-last (:seq diatonic-scale)) start-num)
        octave (range start-num (+ 12 start-num))
        letter->num (fn [l n] (->> (map (fn [a b] [a b])    ;[[:C 0] [:D 2] ...]
                                        pc-cycle-nat-up
                                        (all-notes-in-interval-seq
                                          (:seq diatonic-scale)
                                          0)
                                        )
                                   (filter #(= (keyword l) (first %)))
                                   (map last)
                                   (reduce (fn [acc val] (if (<
                                                               (Math/abs (- val n))
                                                               (Math/abs (- acc n)))
                                                           val acc))
                                           128)))
        accidental (fn [l n] (let [val (- n (letter->num l n))]
                               (cond (= val 0) nil
                                     (= val -1) "b"
                                     (= val 1) "#")))
        in-key? (fn [n] (contains? (set notes-in-key) n))
        hasAccidental? (fn [letter n] (let [pc (note-num->pitch-class n)]
                                        (or (not (natural? pc))
                                            (not= letter pc))))
        start-letter (keyword (first (name (:tonic-name key))))
        num->letter (zipmap notes-in-key
                            (map name
                                 (drop-while #(not= start-letter %)
                                             pc-cycle-nat-up)))
        key-has-sharps? (reduce (fn [acc val] (or acc
                                                  (= "#"
                                                     (accidental
                                                       (num->letter val)
                                                       val))))
                                false
                                notes-in-key)
        spell-note (fn [n] (keyword (if (in-key? n)
                                      (let [letter (num->letter n)]
                                        (str letter (if (hasAccidental? letter n)
                                                      (accidental letter n)
                                                      nil)))
                                      (let [letter (num->letter (first
                                                                  (drop-while
                                                                    #(<= % (if key-has-sharps?
                                                                             n
                                                                             (- n 1)))
                                                                    notes-in-key)))]
                                        (str letter (accidental letter n))
                                        ))))]
    (map spell-note octave)))

(def key-names (keys letter->note-num))

(def key-name->note-spellings (zipmap key-names
                                 (->> key-names
                                      (map #(scale->key diatonic-scale %))
                                      (map spell-notes)
                                      )))

(defn note-num->note-name [note-num key-name]
  (let [note-spellings (key-name key-name->note-spellings)]
    ((zipmap (map letter->note-num
                  note-spellings)
             note-spellings)
      (mod note-num 12)))
  )




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

              :pitch-class-sets                (zipmap (map (partial scale->pcs-name diatonic-scale)
                                                            pitch-classes)
                                                       (map (partial scale->pitch-class-set diatonic-scale)
                                                            pitch-classes))

              :pitch-classes                   pitch-classes
              :pc-cycle-up                     pc-cycle-up
              :pc-cycle-dn                     pc-cycle-dn

              :notes-all                       notes-all
              :notes-ewi                       (into (sorted-set) (range 34 100))

              ;TODO BONUS! write comparator for bright-to-dark
              :mode-names-diatonic-order       [:ionian :dorian :phrygian :lydian :mixolydian :aeolian :locrian]
              :mode-names-bright-to-dark-order [:lydian :ionian :mixolydian :dorian :aeolian :phrygian :locrian]
              })