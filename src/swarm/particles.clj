(ns swarm.particles
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]))
(set-current-implementation :vectorz)

(defprotocol ParticleUpdater
  (update-position [particle]))

(defprotocol ParticleSystemUpdater
  (update-all-positions [system]))

(defrecord Particle [position
                     velocity
                     force-accumulator
                     mass]

  ParticleUpdater
  (update-position [particle]
    (assoc particle :position (+ (:position particle)
                                 (:velocity particle)))))

(defrecord ParticleSystem [particles
                           n-particles
                           simulation-time
                           forces
                           n-forces]

  ParticleSystemUpdater
  (update-all-positions [system]
    (assoc system :particles (map update-position
                                  (:particles system)))))

(defn make-particle [x y z]
  (->Particle
    (array [x y z])
    (array [1 1 0])
    (array [0 0 0])
    5))

(defn init-particle-system [n]
  (->ParticleSystem
    (repeat n (make-particle
                      (rand 3)
                      (rand 4)
                      (rand 5)))
    n
    0
    []
    0))

(def p (make-particle 5 7 9))

(def sys (init-particle-system 5))

(defn -main []
  (println sys))