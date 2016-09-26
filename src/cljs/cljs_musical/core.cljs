(ns cljs-musical.core
  (:require [reagent.core :as r :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [clojure.string :as string]
            [schema.core :as s
             :include-macros true
             ])
  (:use [cljs-musical.glyphs :only [glyphs]]))


(def appstate (r/atom {:t 0}))

(defn loopy [t] (mod (inc t) 10))

(let [interval (.-intervalid js/window)]
  (if interval (js/clearInterval interval))
  (set! (.-intervalid js/window)
        (js/setInterval #(swap! appstate assoc :t (loopy (:t @appstate)))
                        1000)))

(defn clearinterval []
  (js/clearInterval (.-intervalid js/window))
  (set! (.-intervalid js/window) nil))


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


(defn vecstr [v] (string/join " " v))
(def charwidth 0.345)
(def charheight 0.0875)
(def note-z 0.05)

(defn glyph
  ([g pos] (glyph g pos {}))
  ([g pos attrs] [:a-entity (merge {:text     (str "text: " (g glyphs) "; font: Bravura")
                                    :position (vecstr pos)
                                    :material "color: #000"
                                    :key      (str pos g)}
                                   attrs)]))

(defn map-glyphs [gs pos] (map (fn [g i] (glyph g
                                                (assoc pos 0 (+ (first pos)
                                                                (* i charwidth)))))
                               gs (range)))

(defn event->glyphs [event is-above-middle-staff]
  (let [nv (name (:note-value event))
        nv-no-dot (clojure.string/replace nv "." "")
        t (type event)
        tail-dir (if is-above-middle-staff "-dn" "-up")
        glyph-key (keyword (cond
                             (= Rest t) (str "rest-" nv-no-dot)
                             (contains? #{Note Chord} t) (str "note-" nv-no-dot tail-dir)))
        dotted (= "." (last nv))]
    (filter identity [glyph-key
                      (if dotted :note-dot)])))



(defn staff [n] (->> (range 0 n)
                     (map (fn [i] (glyph :staff-5 [(* i charwidth) 0 0] {:material "color: #666"})))))

(defn home-page []
  [:a-scene
   [:a-sun-sky {:material "sunPosition: 1 1 0"}]
   [:a-entity {:position "0 0 -5"}
    (staff 4)
    ;TODO position vertically given note-num and clef
    (map-glyphs (event->glyphs (Note. 60 :2.) true) [0 0 note-z])
    (map-glyphs (event->glyphs (Note. 60 :4) true) [(* 2 charwidth) (* 2 charheight) note-z])
    [:a-entity {:text     (str "text: " (:t @appstate))
                :position "-1 0 -5"
                :abcd     ""
                :material "color: #544"}]
    [:a-entity {:text     "text: \uE1D9; font: Bravura"
                :position "1 0 -5"
                :material "color: #544"}]]
   ])

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
