(ns swarm.particles
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators])
  (:require [clojure.math.numeric-tower :as math]
            [mikera.vectorz.core :as vec-math]))
(set-current-implementation :vectorz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PARTICLE - An implementation of an individual particle.
; The particle record has fields for position, velocity, force and mass.
; The particle implements the ParticleUpdater protocol (i.e. interface), which
; specifies basic operations done to individual particles. The update-particle
; method is the most interesting in that it implements a differential equation
; solver that updates the particle's position and velocity based on the forces
; applied to that particle.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ParticleUpdater
  (update-particle [particle time-step-length])
  (reset-forces [particle])
  (apply-force [particle delta-force]))

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
                                          0)))

  (apply-force [particle delta-force]
    (let [current-forces (:force-accumulator particle)]
      (assoc particle :force-accumulator (+ current-forces delta-force)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FORCE FUNCTIONS - These functions take the particle system state (excluding time
; right now, though that can be easily added) and return the list of particles after
; the force described by the equation has been applied. The lack of time argument
; means that we're only applying forces that do not vary in time. Things like
; gravity, a spring connecting two particles, or walls against which the particles
; bounce.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gravitational constant is 1. Can exlude particles if the field :no-gravity is
; set to true. Implemented that way so not every particle needs a :gravity true
; added to its attributes.
(defn toy-gravity-force [particles]
  (map #(if (:no-gravity %)
          %
          (apply-force % [0 1 0]))
       particles))

; Slightly more complicated. When a spring is added to a system, it needs to know
; which particles to act on. This is accomplished by setting a key in the :spring
; field of the two particles affected by the spring and passing that key to the
; spring function. Alternatively, references to mutable particles could be passed,
; but that seems to prioritize speed over comprehensibility of code. Perhaps it
; will be done as a future optimisation when particles are mutable.
(defn spring-force [spring-key spring-constant damping-constant rest-length]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PARTICLE SYSTEM - Implementation of a system of particles.
; Holds a list of particles, an integer representing simulation time, and a list
; of force functions that determine the particles' fate. It orchestrates the
; simulation, stepping forward in time and applying forces to the particles at
; each step.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Helper functions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn plus-minus [n]
  ((rand-nth [+ -]) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Initializers for systems to play with and sanity check.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    [toy-gravity-force]))

(defn init-spring-system []
  (let [spring-key :s1
        particles (vec (repeatedly 2 #(->Particle
                                        (array [(+ 250 (plus-minus (rand 100)))
                                                (+ 250 (plus-minus (rand 50)))
                                                0])
                                        (array [(plus-minus (rand 2))
                                                (plus-minus (rand 2))
                                                0])
                                        (array [0 0 0])
                                        100)))
        attached-particles (map #(assoc % :spring spring-key)
                                particles)]
    (->ParticleSystem
      attached-particles
      0
      [(spring-force spring-key 0.2 0.2 100)])))

(def sys (init-spring-system))

(defn -main []
  (println sys)
  (println (identity-matrix 3)))