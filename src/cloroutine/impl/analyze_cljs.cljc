(ns cloroutine.impl.analyze-cljs
  (:require [cljs.analyzer]
            [cljs.env]))

(defn analyze [env form]
  (binding [cljs.env/*compiler* (or cljs.env/*compiler*
                                    (cljs.env/default-compiler-env))]
    (cljs.analyzer/analyze env form nil nil)))
