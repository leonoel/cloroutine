# Generators as lazy sequences

In this guide, we'll see how coroutines can be leveraged to build immutable, lazy, possibly infinite sequences defined by imperative processes. This technique is available in [various](https://wiki.python.org/moin/Generators) [languages](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Generator) as syntactic constructs known as `generator`s. The idea is to allow the process to suspend its execution to allow the value to be consumed, an operation known as `yield`ing.

```clojure
(require '[cloroutine.core :refer [cr]])
```

First, let's define a dynamic var to hold the thread-local context keeping track of the generator being evaluated. Each time we run the generator, we bound this var to the tail of the currently generated sequence.

```clojure
(def ^:dynamic *tail*)

(defn gen-seq [gen]
  (binding [*tail* (lazy-seq (gen-seq gen))] (gen)))
```


We can now define the `yield` function in charge of the construction of the sequence from a provided value. Because this is a suspending function, we need to define its associated resume function. As we don't need any useful information to process generation further, the resume function will be a `no-op`.

```clojure
(defn yield [x]
  (cons x *tail*))

(defn no-op [])
```

We can now define the `generator` macro, which just calls `gen-seq` with a fresh coroutine wrapping the body, suspending on `yield` and resuming on `nop`. The body is appended with a `nil` to ensure the sequence is over when the generator terminates.

```clojure
(defmacro generator [& body]
  `(gen-seq (cr {yield no-op} ~@body nil)))
```

The generator machinery is done now, let's define some sequences :

```clojure
(generator
  (yield :a)
  (yield :b)
  (yield :c))                                               ;; returns (:a :b :c)
```

```clojure
(defn my-repeat [x]
  (generator
    (loop []
      (yield x)
      (recur))))

(take 3 (my-repeat 'ho))                                    ;; returns (ho ho ho)
```

```clojure
(defn my-iterate [f x]
  (generator
    (loop [x x]
      (yield x)
      (recur (f x)))))

(take 10 (my-iterate (partial * 2) 1))                      ;; returns (1 2 4 8 16 32 64 128 256 512)
```

```clojure
(def fibonacci
  (generator
    (loop [prev 0 curr 1]
      (yield curr)
      (recur curr (+ prev curr)))))

(take 10 fibonacci)                                         ;; returns (1 1 2 3 5 8 13 21 34 55)
```
