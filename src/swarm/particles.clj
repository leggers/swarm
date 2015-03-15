(ns swarm.particles
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]))
(set-current-implementation :vectorz)

(defn toy-force [particle system]
  (let [current-forces (:force-accumulator particle)]
    (assoc particle :force-accumulator (+ current-forces [0 1 0]))))

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
    (assoc particle :position (+ position
                                 (* velocity time-step-length))
                    :velocity (+ velocity
                                (/ force-accumulator mass))))

  (reset-forces [particle]
    (assoc particle :force-accumulator (* force-accumulator
                                          0))))

(defrecord ParticleSystem [particles
                           simulation-time
                           forces]

  ParticleSystemUpdater
  (step [system]
    (let [next-step-time (inc simulation-time)
          force-reset-particles (map reset-forces particles)
          forced-particles (map #(toy-force % system) force-reset-particles)]
      (assoc system :particles (map #(update-particle % 1)
                                    forced-particles)
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
    0
    []))

(def sys (init-particle-system 5))

(defn -main []
  (println sys)
  (println (identity-matrix 3)))