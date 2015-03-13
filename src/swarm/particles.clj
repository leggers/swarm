(ns swarm.particles
  (:use [clojure.core.matrix]))
(set-current-implementation :vectorz)

(def a (array [1 2 3]))

(defn -main []
  (println a))