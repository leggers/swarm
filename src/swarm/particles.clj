(ns swarm.particles
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]))
(set-current-implementation :vectorz)

; This will be the diff-eq solver.
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

(defn init-particle-system [n]
  (->ParticleSystem
    (repeatedly n #(->Particle
                    [(rand 10)
                     (rand 10)
                     (rand 10)]
                    [0 0 0]
                    [0 0 0]
                    5))
    n
    0
    []
    0))

(def sys (init-particle-system 5))

(defn -main []
  (println sys))