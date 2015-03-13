(ns swarm.particles
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]))
(set-current-implementation :vectorz)

(defprotocol Updater
  (update-position [p]))

(defrecord Particle [position
                     velocity
                     force-accumulator
                     mass]
  Updater
  (update-position [p]
    (assoc p :position (+ (:position p)
                          1))))

(defrecord ParticleSystem [particles
                           n-particles
                           simulation-time
                           forces
                           n-forces])

(def p (->Particle
         (array [1 2 3])
         (array [3 6 7])
         (array [0 0 0])
         5))

(defn -main []
  (println p)
  (println (update-position p)))