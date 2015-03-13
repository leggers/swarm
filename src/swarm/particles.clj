(ns swarm.particles
  (:use [clojure.core.matrix]))
(set-current-implementation :vectorz)

(defrecord Particle [position
                     velocity
                     force-accumulator
                     mass])

(defrecord ParticleSystem [particles
                           n-particles
                           simulation-time
                           forces
                           n-forces])

(def a (array [1 2 3]))

(defn -main []
  (println (->Particle
               (array [1 2 3])
               (array [3 6 7])
               (array [0 0 0])
               5)))