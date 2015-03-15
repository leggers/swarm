(ns swarm.particles
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]))
(set-current-implementation :vectorz)

; This will be the diff-eq solver.
(defprotocol ParticleUpdater
  (update-particle [particle simulation-time]))

(defprotocol ParticleSystemUpdater
  (step [system]))

(defrecord Particle [position
                     velocity
                     force-accumulator
                     mass]

  ParticleUpdater
  (update-particle [particle simulation-time]
    (assoc
      (assoc particle :position (+ (:position particle)
                                   (:velocity particle)))
      :velocity (+ (:velocity particle)
                   0))))

(defrecord ParticleSystem [particles
                           n-particles
                           simulation-time
                           forces
                           n-forces]

  ParticleSystemUpdater
  (step [system]
    (let [next-step-time (inc (:simulation-time system))]
      (assoc
        (assoc system :particles (map #(update-particle % next-step-time)
                                      (:particles system)))
        :simulation-time next-step-time))))

(defn init-particle-system [n]
  (->ParticleSystem
    (repeatedly n #(->Particle
                    [(rand 10)
                     (rand 10)
                     (rand 10)]
                    [(rand 3)
                     (rand 3)
                     (rand 3)]
                    [0 0 0]
                    5))
    n
    0
    []
    0))

(def sys (init-particle-system 5))

(defn -main []
  (println sys))