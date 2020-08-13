(ns aoc19.core
  (:require [aoc19.intcode :as intcode])
  (:gen-class))

(defn -main
  "run intcode program from file"
  [file & args]
  (let [source
        (with-open [r (clojure.java.io/reader file)]
          (binding [*in* r]
            (doall (intcode/read-program))))]
    (intcode/new-program! source)
    (intcode/exe 0)))
