(ns swarm.particles-matrix
  (:use [clojure.core.matrix]
        [clojure.core.matrix.operators])
  (:require [clojure.math.numeric-tower :as math]
            [mikera.vectorz.core :as vec-math]))
(set-current-implementation :vectorz)

; VERY toy system with two particles
; (def masses (* (identity-matrix 6) 2))
; (def positions (array [0 -1 0 1 0 0]))
; (def velocities (array [1 0 0 0 -1 0]))
; (def forces (array [0 0 0 0 0 0]))

(def masses (* (identity-matrix 2) 2))
(def positions (matrix [[0 -1 0]
                        [1 0 0]]))
(def velocities (array [[1 0 0]
                        [0 -1 0]]))
(def forces (array [[0 0 0]
                    [0 0 0]]))

; a = f / m
(defn accelerations [forces masses]
  (mmul (inverse masses)
        forces))

; A real force would return a matrix calculated by multiplying a column vector
; against the positions/velocities/whatever of the particles, passed as arguments.
; This trivial case simply returns a vector that will be broadcast but in general
; force methods should return matricies to add to the existing force matrix
(defn toy-gravity [gravitational-constant]
  (fn [positions velocities]
    [0 gravitational-constant 0]))

(def time-step-size 1)

(defrecord ParticleSystemMatrix [masses
                                 positions
                                 velocities
                                 forces]
  (step [system]
    (assoc system :velocities (+ velocities
                                 (accelerations forces masses))
                  :positions (+ positions
                                (* (:velocities system)
                                   time-step-size))
                  :forces (zero-matrix (shape forces)))))