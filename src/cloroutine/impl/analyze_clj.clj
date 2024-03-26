(ns cloroutine.impl.analyze-clj
  (:require [clojure.tools.analyzer.jvm :as clj])
  (:import (clojure.lang Compiler$LocalBinding)))

(defn analyze [env form]
  (binding [clj/run-passes clj/scheduled-default-passes]
    (->> env
         (into {} (map (fn [[symbol binding]]
                         [symbol (or (when (instance? Compiler$LocalBinding binding)
                                       (let [binding ^Compiler$LocalBinding binding]
                                         {:op   :local
                                          :tag  (when (.hasJavaClass binding)
                                                  (some-> binding (.getJavaClass)))
                                          :form symbol
                                          :name symbol}))
                                     binding)])))
         (update (clj/empty-env) :locals merge)
         (clj/analyze form))))
