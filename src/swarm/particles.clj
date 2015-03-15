(ns swarm.particles
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators])
  (:require [clojure.math.numeric-tower :as math]
            [mikera.vectorz.core :as vec-math]))
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

(defn apply-force [particle delta-force]
  (let [current-forces (:force-accumulator particle)]
    (assoc particle :force-accumulator (+ current-forces delta-force))))

(defn toy-gravity [particles]
  (map #(if (:no-gravity %)
          %
          (apply-force % [0 1 0]))
       particles))

(defn spring [spring-key spring-constant damping-constant rest-length]
  (fn [particles]
    (let [attached-particles (filter #(= (:spring %) spring-key) particles)
          p1 (first attached-particles)
          p2 (second attached-particles)
          displacement (- (:position p1) (:position p2))
          displacement-magnitude (vec-math/magnitude displacement)
          displacement-force (* spring-constant (- displacement-magnitude rest-length))
          velocity-difference (- (:velocity p1) (:velocity p2))
          damping-force (* damping-constant
                           (/ (dot velocity-difference displacement) displacement-magnitude))
          force-direction (normalise displacement)
          force-magnitude (+ displacement-force damping-force)
          force-1 (- (* force-direction force-magnitude))
          force-2 (- force-1)]
      (map #(cond
              (= % p1) (apply-force % force-1)
              (= % p2) (apply-force % force-2)
              :else %)
           particles))))

(defprotocol ParticleSystemUpdater
  (update-forces [system])
  (step [system]))

(defrecord ParticleSystem [particles
                           simulation-time
                           forces]

  ParticleSystemUpdater
  ; Resets forces from previous step and evaluates forces for current step.
  (update-forces [system]
    (loop [current-particles (map reset-forces particles)
           force-to-apply (first forces)
           forces-left (rest forces)]
      (if (nil? force-to-apply)
        current-particles
        (recur (force-to-apply current-particles)
               (first forces-left)
               (rest forces-left)))))

  (step [system]
    (let [time-step-size 1
          next-step-time (+ simulation-time time-step-size)
          forced-particles (update-forces system)]
      (assoc system :particles (map #(update-particle % time-step-size)
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

(defn init-spring-system []
  (let [spring-key :s1
        particles (vec (repeatedly 2 #(->Particle
                                        (array [(+ 250 (rand 100))
                                                250
                                                0])
                                        (array [0
                                                0
                                                0])
                                        (array [0 0 0])
                                        100)))
        attached-particles (map #(assoc % :spring spring-key)
                                particles)]
    (->ParticleSystem
      attached-particles
      0
      [(spring spring-key 0.2 1 100)])))

(def sys (init-spring-system))

(defn -main []
  (println sys)
  (println (identity-matrix 3)))