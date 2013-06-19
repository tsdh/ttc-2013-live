(ns ^{:pattern-expansion-context :tg}
  ttc-2013-live.core
  (:use funnyqt.protocols
        funnyqt.emf
        funnyqt.query.emf
        funnyqt.declarative
        funnyqt.pmatch
        funnyqt.in-place)
  (:require [funnyqt.visualization :as viz]))

;;* Load the metamodel and model

(load-metamodel "TODO")
(def m (load-model "TODO"))
