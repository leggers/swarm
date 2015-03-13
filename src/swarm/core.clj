(ns swarm.core
  (:require [swarm.particles :as particles]))

(def particle-system (particles/init-particle-system 5))

(defn -main []
  (println particle-system))