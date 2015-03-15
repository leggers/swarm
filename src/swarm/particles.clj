(ns swarm.particles
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators]))
(set-current-implementation :vectorz)

(defprotocol ParticleUpdater
  (update-particle [particle simulation-time])
  (reset-forces [particle]))

(defrecord Particle [position
                     velocity
                     force-accumulator
                     mass]

  ; Use Euler's method for now
  ParticleUpdater
  (update-particle [particle time-step-length]
    (assoc particle :position (+ position
                                 (* velocity time-step-length))
                    :velocity (+ velocity
                                (/ force-accumulator mass))))

  (reset-forces [particle]
    (assoc particle :force-accumulator (* force-accumulator
                                          0))))


(defn toy-gravity [system]
  (map #(let [current-forces (:force-accumulator %)]
          (if (:no-gravity %)
            %
            (assoc % :force-accumulator (+ current-forces [0 1 0]))))
       system))

(defprotocol ParticleSystemUpdater
  (apply-forces [system])
  (step [system]))

(defrecord ParticleSystem [particles
                           simulation-time
                           forces]

  ParticleSystemUpdater
  (apply-forces [system]
    (loop [current-particles particles
           force-to-apply (first forces)
           forces-left (rest forces)]
      (if (nil? force-to-apply)
        current-particles
        (recur (force-to-apply current-particles)
               (first forces-left)
               (rest forces-left)))))

  (step [system]
    (let [next-step-time (inc simulation-time)
          force-reset-particles (map reset-forces particles)
          forced-particles (apply-forces system)]
      (assoc system :particles (map #(update-particle % 1)
                                    forced-particles)
                    :simulation-time next-step-time))))

(defn init-particle-system [n]
  (->ParticleSystem
    (vec (repeatedly n #(->Particle
                    (array [(rand 10)
                            (rand 10)
                            (rand 10)])
                    (array [(rand 10)
                            (rand 5)
                            (rand 3)])
                    (array [0 0 0])
                    500)))
    0
    [toy-gravity]))

(def sys (init-particle-system 5))

(defn -main []
  (println sys)
  (println (identity-matrix 3)))