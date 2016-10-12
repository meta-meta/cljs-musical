(ns cljs-musical.core
  (:require [reagent.core :as r :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [clojure.string :as string]
            [schema.core :as s
             :include-macros true
             ])
  (:use [cljs-musical.glyphs :only [glyphs]]
        [cljs-musical.library :only [note-num->note-name pc-cycle-nat-up pc-cycle-nat-dn]]))


(def appstate (r/atom {:t 0
                       :midi-notes {}}))

(defn loopy [t] (mod (inc t) 10))

(let [interval (.-intervalid js/window)]
  (if interval (js/clearInterval interval))
  (set! (.-intervalid js/window)
        (js/setInterval #(swap! appstate assoc :t (loopy (:t @appstate)))
                        1000)))

(defn clearinterval []
  (js/clearInterval (.-intervalid js/window))
  (set! (.-intervalid js/window) nil))


(def midi (r/atom {:inputs {}}))
(.then (.requestMIDIAccess (.-navigator js/window))
       (fn [res] (.forEach (.-inputs res)
                           #(swap! midi assoc-in [:inputs (.-name %)] %))))

(defn list-midi-inputs [] (keys (:inputs @midi)))
(defn midi-listen [input-name] (set! (.-onmidimessage (get-in @midi [:inputs input-name]))
                                     (fn [msg] (let [data (.-data msg)
                                                     channel (aget data 0)
                                                     note-num (aget data 1)
                                                     velocity (aget data 2)]
                                                 ;(if (= channel 146))
                                                 (swap! appstate assoc-in [:midi-notes note-num] velocity)))))
(midi-listen "USB Axiom 49")
;(midi-listen "EWI-USB")
;(midi-listen "Keystation Pro 88")




(defn rgb->hex [r g b] (str "#" (.slice (.toString (js/Number (+ (bit-shift-left 1 24)
                                                                 (bit-shift-left (min r 255) 16)
                                                                 (bit-shift-left (min g 255) 8)
                                                                 (min b 255)))
                                                   16)
                                        1)))
(defn rgbn->hex [r g b] (rgb->hex (* 255 r) (* 255 g) (* 255 b)))

(defn tinycolor [string] (.toHexString (js/tinycolor string)))

(defn hsl->hex [h s l] (tinycolor (str "hsl " (* 255 h) " " s " " l)))
;tinycolor('hsl 0.5 1.0 0.5').toHexString()


;; PRESENTATION DATA TYPES

;; on the presentation side of things, as opposed to the library, we only care about the output data. not so much the theory that generated it.


(def NoteValue (s/pred (fn [x] (and (keyword? x)
                                    (contains? (set (map #(Math/pow 2 %)
                                                         (range 6)))
                                               (js/parseFloat (name x)))))
                       "is a keyword of integer between 1 and 32 representing divisor of a whole note. a dot may follow which adds 1/2 the note-value"))

(s/defrecord Note
  [data :- s/Num
   note-value :- NoteValue])

(s/defrecord Rest
  [note-value :- NoteValue])

(s/defrecord Chord
  [data :- #{s/Num}
   note-value :- NoteValue])

(s/defrecord Clef
  [glyph :- s/Keyword
   bottom-note :- s/Num                                     ; the note at bottom of staff
   staff-offset :- s/Num]                                   ; vertical position of glyph on staff
  )




(def g-clef (Clef. :clef-g 64 2))                           ; bottom-note E5
(def f-clef (Clef. :clef-f 43 6))                           ; bottom-note G3

(defn vecstr [v] (string/join " " v))                       ; [1 2 3] -> "1 2 3"

(def charwidth 0.345)
(def charheight 0.0875)
(def note-dx (* 3 charwidth))                               ; width of one note with/without dot
(def note-dy charheight)                                    ; height of one note
(def note-z 0.05)                                           ; placement of note forward from staff

(defn text
  ([g pos] (text g pos {}))
  ([t pos attrs] [:a-entity (merge {:text     (str "text: " t ";")
                                    :position (vecstr pos)
                                    :material "color: #666"
                                    :key      (str pos t)}
                                   attrs)]))

(defn glyph
  ([g pos] (glyph g pos {}))
  ([g pos attrs] [:a-entity (merge {:text     (str "text: " (glyphs g) "; font: Bravura")
                                    :position (vecstr pos)
                                    :material "color: #990"
                                    :key      (str pos g)}
                                   attrs)]))

(defn staff [n] (->> (range 0 (* 3 n))                      ; 3 possible chars per segment: accidental, note, dot
                     (map (fn [i] (glyph :staff-5 [(* i charwidth) 0 0] {:material "color: #666"})))))

(defn clef [c] (glyph (:glyph c) [0 (* note-dy (:staff-offset c)) 0] {:material "color: #666"}))


(defn event
  ([e clef key-name] (event e clef key-name {}))
  ([e clef key-name attrs]
   (let [t (type e)
         clef-bottom-note-name (note-num->note-name (:bottom-note clef) :C)
         note-name-cycle-up (drop-while #(not= clef-bottom-note-name %) pc-cycle-nat-up)
         note-name-cycle-dn (drop-while #(not= clef-bottom-note-name %) pc-cycle-nat-dn)
         note->y-pos (fn [n] (let
                               [dist-from-bottom (- n (:bottom-note clef)) ;distance in note-num
                                octaves-from-bottom (int (/ dist-from-bottom 12))
                                note-name-cycle (if (< dist-from-bottom 0)
                                                  note-name-cycle-dn
                                                  note-name-cycle-up)
                                note-name-dy-map (map (fn [a b] ; ([:E 0] [:F 1] [:G 2] [:A 3] [:B 4] ...)
                                                        [a (if (< dist-from-bottom 0)
                                                             (* -1 b)
                                                             b)])
                                                      note-name-cycle (range))
                                natural-note-name (keyword (first (name (note-num->note-name n key-name))))

                                ]
                               (+ (* octaves-from-bottom 7)
                                  (last                     ; 2
                                    (first                  ; [:G 2]
                                      (drop-while #(not= (first %) ; cycle note-names until match
                                                         natural-note-name)
                                                  note-name-dy-map)))
                                  )
                               ))
         is-above-middle-staff (fn [y-pos] (>= y-pos 4))
         note->accidental (fn [n] (let [maybe-accidental (last (name
                                                                 (note-num->note-name
                                                                   n
                                                                   key-name)))]
                                    (cond
                                      (= "#" maybe-accidental) :sharp
                                      (= "b" maybe-accidental) :flat
                                      :default :space)))
         event->glyphs (fn [event is-above-middle-staff]
                         (let [nv (name (:note-value event))
                               nv-no-dot (clojure.string/replace nv "." "")
                               tail-dir (if is-above-middle-staff "-dn" "-up")
                               glyph-key (keyword
                                           (cond
                                             (= Rest t) (str "rest-" nv-no-dot)
                                             (contains? #{Note Chord} t) (str "note-" nv-no-dot tail-dir)))
                               dotted (= "." (last nv))]
                           (filter identity [glyph-key
                                             (if dotted :note-dot)])))
         map-glyphs (fn [gs pos] (map (fn [g i] (glyph g
                                                       (assoc pos 0 (+ (first pos)
                                                                       (* i charwidth)))
                                                       (merge {:key (str g i)} attrs)))
                                      gs (range)))
         y-positions (cond
                       (= Rest t) #{[4 " "]}
                       (= Note t) (map (fn [n] [(note->y-pos n) (note->accidental n)]) #{(:data e)})
                       (= Chord t) (map (fn [n] [(note->y-pos n) (note->accidental n)]) (:data e)))
         evt-glyphs (event->glyphs e (is-above-middle-staff (first (first y-positions)))) ; (first (first ([4 :flat])))
         ]

     (map (fn [y-pos] (map-glyphs (cons (last y-pos) evt-glyphs) [0 (* (first y-pos) note-dy) note-z]))
          y-positions)
     ; TODO: ledger lines
     )))


(defn spiral [midi-notes radius]
  (let [sphere (fn [note-num velocity]
                 (let [theta (* Math/PI 2 (/ (mod note-num 12) 12))
                       octave (/ note-num 12)
                       is-placeholder (= 0 velocity)]
                   [:a-sphere {:radius   (if is-placeholder 0.01 (/ velocity 255))
                               :position (vecstr [(* radius (Math/sin theta))
                                                  octave
                                                  (* radius (Math/cos theta))])
                               :color    (hsl->hex (/ theta (* 2 Math/PI))
                                                   1
                                                   0.5)}]))
        midi-note->sphere (fn [midi-note]
                            (let [note-num (first midi-note)
                                  velocity (last midi-note)]
                              (sphere note-num velocity)))]

    [:a-entity {}
     (map midi-note->sphere midi-notes)
     (map sphere (range 128) (repeat 0))
     ]
    ))

(defn midi-staff [midi-notes key-name my-clef]
  [:a-entity {}
   (staff 1)
   (map (fn [midi-note] (let [note-num (first midi-note)
                              velocity (last midi-note)]
                          (event (Note. note-num :2)
                                 my-clef
                                 key-name
                                 {:material (str "color: " (hsl->hex (- 0.5 (/ velocity 255))
                                                                     (/ velocity 128)
                                                                     0.6))})))
        midi-notes)])

(defn home-page []
  (let [key-name :C
        t (:t @appstate)
        sunpos (vecstr [1 0 -1])
        midi-notes (into {} (filter #(> (last %) 0) (:midi-notes @appstate)))]
    [:a-scene
     [:a-sky {:color (rgb->hex 20 0 24)}]
     ;[:a-sun-sky {:material (str "shader: sunSky; sunPosition: " sunpos)}]
     ;[:a-box {:position (vecstr [(- (:t @appstate) 5) 0 -15]) :material "color: #000"}]


     ;; SPIRAL
     #_[:a-entity {:position "5 -10 -5"}
      (spiral midi-notes 0.5)]

     ;; "Screen"
     [:a-entity {:position "0 0 -5"}
      (text (str "key of " key-name) [-4 0 0] {:scale (vecstr [0.5 0.5 0.5])}) ; display the key
      (text (str "t: " (:t @appstate)) [-4 -0.5 0] {:scale (vecstr [0.5 0.5 0.5])}) ; display the time


      ;; Treble Clef
      (let [my-clef g-clef]
        [:a-entity {:position (vecstr [0 0 0])}

         [:a-entity {:position (vecstr [(* -1 note-dx) 0 0])} ; g clef
          (staff 1)
          (clef my-clef)]


         ;(staff 1)                                          ;C60 treble cleg
         ;(event (Note. 60 :2) my-clef key-name)


         (midi-staff (into {} (filter #(>= (first %) 60) midi-notes)) key-name my-clef)


         ;[:a-entity                                         ; 1 chord
         ; (staff 1)
         ; (event (Chord. #{60 64 69} :2.) my-clef key-name)]
         ;[:a-entity {:position (vecstr [(* 1 note-dx) 0 0])} ; 2 chord
         ; (staff 1)
         ; (event (Chord. #{62 66 73} :4) my-clef key-name)]
         ;[:a-entity {:position (vecstr [(* 2 note-dx) 0 0])} ; 3 note
         ; (staff 1)
         ; (event (Note. 80 :4) my-clef key-name)]
         ]
        )

      ;; Bass Clef
      #_(let [my-clef f-clef]
        [:a-entity {:position (vecstr [0 (* -12 note-dy) 0])}

         [:a-entity {:position (vecstr [(* -1 note-dx) 0 0])} ; f clef
          (staff 1)
          (clef my-clef)]

         ;(staff 1)                                          ;C60 bass clef
         ;(event (Note. 60 :2) my-clef key-name)

         (midi-staff (into {} (filter #(< (first %) 60) midi-notes)) key-name my-clef)


         ;[:a-entity                                         ; 1 chord
         ; (staff 1)
         ; (event (Chord. #{40 44 48} :2.) my-clef key-name)]
         ;[:a-entity {:position (vecstr [(* 1 note-dx) 0 0])} ; 2 chord
         ; (staff 1)
         ; (event (Chord. #{42 46 53} :4) my-clef key-name)]
         ;[:a-entity {:position (vecstr [(* 2 note-dx) 0 0])} ; 3 note
         ; (staff 1)
         ; (event (Note. 60 :4) my-clef key-name)]
         ])

      ]
     ]))

(defn about-page []
  [:div [:h2 "About cljs-musical"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
                    (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
                    (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
