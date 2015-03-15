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
          blah (println attached-particles)
          p1 (first attached-particles)
          p2 (second attached-particles)
          displacement (- (:position p1) (:position p2))
          displacement-magnitude (vec-math/magnitude displacement)
          displacement-force (* spring-constant (- displacement-magnitude rest-length))
          velocity-difference (- (:velocity p1) (:velocity p2))
          damping-force (/ (dot velocity-difference displacement) displacement-magnitude)
          force-direction (normalise displacement)
          force-1 (- (* (+ displacement-force damping-force) force-direction))
          force-2 (- force-1)
          spring-key (:spring p1)]
      (map #(cond
              (= % p1) (apply-force % force-1)
              (= % p2) (apply-force % force-2)
              :else %)
           particles))))

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

(defn init-spring-system []
  (let [spring-key :s1
        particles (vec (repeatedly 2 #(->Particle
                                        (array [(rand 100)
                                                (rand 50)
                                                0])
                                        (array [0
                                                (rand 1)
                                                0])
                                        (array [0 0 0])
                                        1000)))
        attached-particles (map #(assoc % :spring spring-key)
                                particles)]
    (->ParticleSystem
      attached-particles
      0
      [(spring spring-key 0.1 10 10)])))

(def sys (init-spring-system))

(defn -main []
  (println sys)
  (println (identity-matrix 3)))