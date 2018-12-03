# Transducers revisited

In this guide, we'll reimplement [clj-conduit](https://github.com/hypirion/clj-conduit) using `cloroutine`. If you haven't already, I strongly recommend to have a look at the library and the associated [post](http://hypirion.com/musings/transducers-to-conduits-and-back) before going further. A basic understanding of the anatomy of a transducer is required as well.

The idea of `conduit` is that custom transducers could be much easier to write with a dedicated syntax able to emulate waiting on input. Basically, we want to be able to define a transducer as a block of code in which we would be allowed to use special operators to produce (`yield`) and consume (`await`) values in imperative style.

```clojure
(require '[cloroutine.core :refer [cr]])
```

First, let's define some dynamic vars to hold thread-local context available during evaluation of conduit blocks.
* `*acc*` is the accumulator of the reducing process.
* `*down*` is the downstream reducing function
* `*input*` is the upstream value currently processed.

```clojure
(def ^:dynamic *acc*)
(def ^:dynamic *down*)
(def ^:dynamic *input*)
```

We can now define the functions in charge of I/O in `conduit`s.
* `yield`ing an output value is a synchronous operation, all we have to do is to reduce the accumulator with given value and check for early termination.
* `await`ing an input is more tricky because it requires to suspend the process to release control to the transducing context. The function takes as argument a sentinel value to return in case of end-of-stream, when the process requires a finalization step. Otherwise the function can be called with zero argument and the process will be resumed only on available input.
* `input` is the resume function associated with `await`, its sole job is to retrieve input value from context.

```clojure
(defn yield [x]
  (not (reduced? (set! *acc* (*down* *acc* x)))))

(defn await
  ([] (await ::done))
  ([eos] eos))

(defn input []
  *input*)
```

Now, let's define the function `conduit-xf` building the transducer itself. The conduit process is wrapped in a coroutine, itself wrapped in a constructor function. The coroutine is instanciated on transducing context initialization, along with a mutable box to keep track of the end-of-stream sentinel value between two successive steps.

```clojure
(defn conduit-xf [ctor]
  (fn [down]
    (let [cor (ctor)
          eos (volatile! (cor))]
      (fn rf
        ([]
         (down))
        ([acc]
         (down (case @eos
                 ::done acc
                 (rf acc @eos))))
        ([acc x]
         (binding [*acc*   acc
                   *down*  down
                   *input* x]
           (vreset! eos (cor))
           *acc*))))))
```

The `conduit` macro now simply consists of wrapping a body in a coroutine, ensuring early termination of the reduction process when done.
```clojure
(defmacro conduit [& body]
  `(conduit-xf
     #(cr {await input}
        ~@body (set! *acc* (ensure-reduced *acc*)) ::done)))
```

Additionally, we can define syntactic sugar on top of `await`, allowing to branch on end-of-stream without having to provide and test against a sentinel.
```clojure
(defmacro if-let-await [sym then else]
  `(let [x# (await ::over)]
     (case x#
       ::over ~else
       (let [~sym x#] ~then))))
```

As an example of usage, here are the reimplementations of clojure.core's transducers, stolen from [clj-conduit](https://github.com/hyPiRion/clj-conduit/wiki/clojure.core-ports).

```clojure
(defn mapping [f]
  (conduit
    (while true
      (yield (f (await))))))

(defn mapping-indexed [f]
  (conduit
    (loop [i 0]
      (yield (f i (await)))
      (recur (inc i)))))

(defn filtering [pred]
  (conduit
    (while true
      (let [val (await)]
        (when (pred val)
          (yield val))))))

(defn taking-while [pred]
  (conduit
    (loop []
      (let [val (await)]
        (when (pred val)
          (yield val)
          (recur))))))

(defn taking [n]
  (conduit
    (dotimes [_ n]
      (yield (await)))))

(defn taking-nth [n]
  (conduit
    (while true
      (yield (await))
      (dotimes [_ (- n 1)]
        (await)))))

(defn dropping [n]
  (conduit
    (dotimes [_ n]
      (await))
    (while true
      (yield (await)))))

(defn dropping-while [pred]
  (conduit
    (loop [v (await)]
      (if (pred v)
        (recur (await))
        (yield v)))
    (while true
      (yield (await)))))

(def catting
  (conduit
    (while true
      (doseq [val (await)]
        (yield val)))))

(defn mapcatting [f]
  (conduit
    (while true
      (doseq [val (f (await))]
        (yield val)))))

(def deduping
  (conduit
    (loop [old ::none]
      (let [new (await)]
        (when-not (= old new)
          (yield new))
        (recur new)))))

(defn replacing [smap]
  (conduit
    (while true
      (let [val (await)]
        (yield (get smap val val))))))

(defn keeping [f]
  (conduit
    (while true
      (let [v (f (await))]
        (when-not (nil? v)
          (yield v))))))

(defn keeping-indexed [f]
  (conduit
    (loop [i 0]
      (let [v (f i (await))]
        (when-not (nil? v)
          (yield v)))
      (recur (inc i)))))

(def distincting
  (conduit
    (loop [seen #{}]
      (let [val (await)]
        (if-not (contains? seen val)
          (do (yield val)
              (recur (conj seen val)))
          (recur seen))))))

(defn random-sampling [prob]
  (conduit
    (while true
      (let [val (await)]
        (when (< (rand) prob)
          (yield val))))))

(defn interposing [sep]
  (conduit
    (yield (await))
    (while true
      (let [val (await)]
        (yield sep)
        (yield val)))))

(defn partitioning-all [n]
  (conduit
    (loop [vs [(await)]]
      (if (= n (count vs))
        (do (yield vs)
            (recur [(await)]))
        (if-let-await v
          (recur (conj vs v))
          (yield vs))))))

(defn partitioning-by [f]
  (conduit
    (let [first-val (await)]
      (loop [vs     [first-val]
             to-cmp (f first-val)]
        (if-let-await v
          (let [new-to-cmp (f v)]
            (if (= to-cmp new-to-cmp)
              (recur (conj vs v) to-cmp)
              (do (yield vs)
                  (recur [v] new-to-cmp))))
          (yield vs))))))
```
