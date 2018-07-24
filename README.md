# cloroutine

A generic coroutine builder for clojure.

## Status

Maturity : alpha

Artifacts are released to [clojars](https://clojars.org/cloroutine).

Current coordinates : `[cloroutine "a.1"]`

## Rationale

As discussed in this [thread](https://clojureverse.org/t/async-generator-functions-in-cljs-requesting-feedback/2262), recurring complaints about clojurescript relate to the lack of alternatives for javascript's latest coroutine-based features, especially the trendy promise-based async/await pattern.
No consensus has been reached yet on whether and how this style should impact clojure and clojurescript, which is a good that sign that more hammock time has to be spent.
In order to fuel the discussion and challenge this idea, this project provides a generic solution to coroutine problems as a single third-party macro.

## Limitations

The implementation is basically a polished version of `core.async`'s IOC machinery and thus has the same limitations as the `go` macro.

## Usage

This library exposes a single macro `cloroutine.core/cr` taking a vector of control flow definitions followed by a body of expressions, and returns a stateful function able to sequentially evaluate the body, pausing and resuming execution according to control flow definitions.

Expression in first position in the vector will be evaluated each time execution is resumed, and the result of this evaluation will be the result of the block responsible for pausing execution.

Expressions after the first in the vector must be function definitions (following `letfn` syntax) starting with a fully-qualified symbol. Each call to functions referenced by these symbols in the body will execute the matching block instead, ignoring result of evaluation, then pause execution.

Returned function takes a sentinel value and returns this value if execution was suspended, or the result of last expression if end of body was reached.

## Examples
* define lazy sequences from imperative-style generator syntax

```clojure
(ns user
  (:require [cloroutine.core :refer [cr]]))

(defn yield [_]
  (throw (ex-info "Unable to yield : not in generator" {})))

(defmacro generator [& body]
  `(let [curr# (object-array 1)
         feed# (cr [(aget curr# 0)
                    (user/yield [x#] (aset curr# 0 x#))]
                 ~@body)]
     ((fn rec# []
        (lazy-seq
          (when (identical? feed# (feed# feed#))
            (cons (aget curr# 0) (rec#))))))))

(def fib
  (generator
    (loop [prev 0 curr 1]
      (recur (yield curr) (+ prev curr)))))

(take 10 fib)                   ;; returns (1 1 2 3 5 8 13 21 34 55)
```

* async/await syntax for javascript promises

```clojure
(ns user
  (:refer-clojure :exclude [await])
  (:require [cloroutine.core :refer [cr]])
  #?(:cljs (:require-macros [user :refer [async]])))

(defn await [_]
  (throw (ex-info "Unable to await : not in async" {})))

(defmacro async [& body]
  `(js/Promise.
     (fn [resolve# reject#]
       (let [state# (object-array 2)]
         ((aset state# 0
                (cr [((aget state# 1))
                     (user/await [p#]
                       (.then p#
                         (fn [x#]
                           (aset state# 1 #(do x#))
                           ((aget state# 0) nil))
                         (fn [e#]
                           (aset state# 1 #(throw e#))
                           ((aget state# 0) nil))))]
                  (try (resolve# (do ~@body)) (catch :default e# (reject# e#))))) nil)))))

#?(:cljs
(let [six (async 6)                                          ;; => a promise fulfilled with 6
      seven (async (inc (await six)))                        ;; => a promise fulfilled with 7
      failure (async (throw :error))]                        ;; => a promise rejected with :error
  (async (prn (* (await six) (await seven))))                ;; => eventually prints 42
  (async (try (await failure) (catch :default e (prn e)))))) ;; => eventually prints error
```

## License

Licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html) (the same as Clojure)
