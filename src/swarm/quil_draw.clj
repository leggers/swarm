(ns swarm.quil-draw
  (:require [quil.core :as q]
            [swarm.particles :as particles]))

(def diameter 10)

(defn run-system []
  (swap! (q/state-atom)
         update-in
         [:particle-system]
         #(particles/update-all-positions %)))

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
  (q/frame-rate 30)
  (q/stroke 100)
  (q/fill 50)
  (q/stroke-weight 4)
  (q/set-state! :particle-system (particles/init-particle-system 5)))

(defn draw []
  (let [particles-list (:particles (q/state :particle-system))]
    (q/background 200)
    (draw-particles particles-list)
    (run-system)))

(defn -main []
  (q/defsketch particle-test
    :title "Particles!!!!"
    :setup setup
    :draw draw
    :size [500 500]))