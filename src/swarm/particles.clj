(ns swarm.particles
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]))
(set-current-implementation :vectorz)

(defn toy-force [particle system]
  (let [current-forces (:force-accumulator particle)]
    (assoc particle :force-accumulator (+ current-forces [0 1 0]))))

; This will be the diff-eq solver.
(defprotocol ParticleUpdater
  (update-particle [particle simulation-time])
  (reset-forces [particle]))

(defprotocol ParticleSystemUpdater
  (step [system]))

(defrecord Particle [position
                     velocity
                     force-accumulator
                     mass]

  ParticleUpdater
  (update-particle [particle time-step-length]
    (let [position (:position particle)
          velocity (:velocity particle)
          forces (:force-accumulator particle)]
     (assoc
      (assoc particle :position (+ position
                                   (* velocity time-step-length)))
      :velocity (+ velocity
                   (/ forces mass)))))
  (reset-forces [particle]
    (assoc particle :force-accumulator (* (:force-accumulator particle)
                                          0))))

(defrecord ParticleSystem [particles
                           n-particles
                           simulation-time
                           forces
                           n-forces]

  ParticleSystemUpdater
  (step [system]
    (let [next-step-time (inc (:simulation-time system))
          curr-particles (:particles system)
          force-reset-particles (map reset-forces particles)
          forced-particles (map #(toy-force % system) force-reset-particles)]
      (assoc
        (assoc system :particles (map #(update-particle % 1)
                                      forced-particles))
        :simulation-time next-step-time))))

(defn init-particle-system [n]
  (->ParticleSystem
    (repeatedly n #(->Particle
                    (array [(rand 10)
                            (rand 10)
                            (rand 10)])
                    (array [(rand 3)
                            (rand 3)
                            (rand 3)])
                    (array [0 0 0])
                    50))
    n
    0
    []
    0))

(def sys (init-particle-system 5))

(defn -main []
  (println sys)
  (println (identity-matrix 3)))