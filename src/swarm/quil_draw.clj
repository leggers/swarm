(ns swarm.quil-draw
  (:require [quil.core :as q]
            [swarm.particles :as particles]
            [clojure.math.numeric-tower :as math]))

(def diameter 10)
(def steps-per-color 20)

(defn rainbow-color [step rainbow]
  (let [colors-in-rainbow (count rainbow)
        color-index (math/floor (/ step steps-per-color))
        color-1 (get rainbow (mod color-index colors-in-rainbow))
        color-2 (get rainbow (mod (inc color-index) colors-in-rainbow))
        color-distance (/ (mod step steps-per-color)
                          steps-per-color)]
    (q/lerp-color color-1 color-2 color-distance)))

(defn run-system []
  (swap! (q/state-atom)
         update-in
         [:particle-system]
         #(particles/step %)))

(defn draw-particles [particles-list]
  (dorun
    (map
      #(let [position (:position %)]
         (q/ellipse (first position)
                    (second position)
                    diameter
                    diameter))
      particles-list)))

(defn setup []
  (q/smooth)
  (q/frame-rate 60)
  (q/background 0)
  (def red (q/color 255 0 0))
  (def orange (q/color 255 165 0))
  (def yellow (q/color 255 255 0))
  (def green (q/color 0 255 0))
  (def blue (q/color 0 0 255))
  (def indigo (q/color 75 0 130))
  (def violet (q/color 238 130 238))
  (def rainbow [red orange yellow green blue indigo violet])
  (q/set-state! :particle-system (particles/init-particle-system 500))
  ; (swap! (q/state-atom)
  ;        update-in
  ;        [:particle-system :particles 3]
  ;        #(assoc % :no-gravity true))
  (println (q/state-atom)))

(defn draw []
  ; (println (q/state-atom))
  (let [particle-system (q/state :particle-system)
        particles-list (:particles particle-system)
        step (:simulation-time particle-system)
        color (rainbow-color step rainbow)]
    (q/fill color)
    ; (q/background 200)
    (q/stroke color)
    (draw-particles particles-list)
    (run-system)))

(defn -main []
  (q/defsketch particle-test
    :title "Particles!!!!"
    :setup setup
    :draw draw
    :size [750 750]))