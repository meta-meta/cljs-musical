(ns cljs-musical.core
  (:require [reagent.core :as r :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [clojure.string :as string])
  (:use [cljs-musical.glyphs :only [glyphs]]))


(def appstate (r/atom {:t 0}))

(defn loopy [t] (mod (inc t) 10))

(def jsinterval
  (js/setInterval #(swap! appstate assoc :t (loopy (:t @appstate))) 500))

(defn clearinterval [] (js/clearInterval jsinterval))

(defn glyph [g pos] [:a-entity {:text     (str "text: " (g glyphs) "; font: Bravura")
                            :position (string/join " " pos)
                            :material "color: #000"}])

(defn staff [n] (->> (range 0 n) (map (fn [i] (glyph :staff-5 [i 0 -10])))))

(defn home-page []
  [:a-scene
   (staff 10)
   (glyph :staff-5 [0 0 -5])
   [:a-entity {:text     (str "text: " (:t @appstate))
               :position "-1 0 -5"
               :abcd ""
               :material "color: #544"}]
   [:a-entity {:text     "text: \uE1D9; font: Bravura"
               :position "1 0 -5"
               :material "color: #544"}]
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
