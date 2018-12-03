# cloroutine

A generic, macro-based, stackless coroutine builder for Clojure and ClojureScript.

[![clojars](https://img.shields.io/clojars/v/cloroutine.svg)](https://clojars.org/cloroutine)

[![cljdoc](https://cljdoc.org/badge/cloroutine/cloroutine)](https://cljdoc.org/d/cloroutine/cloroutine/CURRENT)

[![build](https://travis-ci.org/leonoel/cloroutine.svg?branch=master)](https://travis-ci.org/leonoel/cloroutine)

[![license](https://img.shields.io/github/license/leonoel/cloroutine.svg)](LICENSE)


## Rationale

Coroutines are syntactic constructs allowing to suspend a computation and resume it later from the point it was suspended. They provide a basis for solutions to various categories of problems. In particular, this strategy has shown notable expressivity improvements in asynchronous programming, sequence generation and data processing.

This library aims to capture the essence of the inversion-of-control mechanism at work in those solutions. Because applications of this programming style are various and still in active exploration, this library is intentionaly low-level and agnostic wrt concurrency. It provides no execution model, exposing bare unsynchronized mutable objects.

The reason for this choice is that coroutine-based programming is inherently imperative, and providing thread-safe imperative constructs requires to make opinionated choices impacting performance. `cloroutine` aims to be a simple and generic tool and thus delegates these choices to third-party library designers.


## Reference

This library exposes a single namespace holding a single macro : [`cloroutine.core/cr`](src/cloroutine/core.cljc)


## Guides

The following guides show how to leverage the `cr` macro to implement clojure-flavored versions of various syntactic constructs involving suspendable processes.
1. [Generators as lazy sequences](doc/01-generators.md)
2. [Future-based asynchronous processes, aka async/await](doc/02-async-await.md)
3. [Transducers revisited](doc/03-conduits.md)
