# Monads

A monad refers to a category of values associated with a pair of functions `[unit bind]` such that :
* `(unit x)` returns a member of this category given any value `x`
* `(bind f m)` returns a member of this category given any other member `m` of this category and a function `f` taking a single argument and returning a member of this category.
* `(bind f (unit x))` is equivalent to `(f x)`
* `(bind unit m)` is equivalent to `m`
* `(->> m (bind f) (bind g))` is equivalent to `(bind #(bind g (f %)) m)`

Some monads :
```clojure
;; clojure sequences are monadic out of the box.
(def seqable-monad [list mapcat])

;; a monad short-circuiting computation on nil.
(def nilable-monad [identity (fn [f x] (when (some? x) (f x)))])
```

The monad abstraction allows to build very flexible syntactic constructs to express sequential logic. For instance, let's see how we could generalize clojure's `for` notation (the simple part, at least) to any monad and have a notation similar to haskell's `do` (`do` has a different meaning in clojure so it'll be `mlet`, for monadic let).

```clojure
(defmacro mlet [monad bindings & body]
  (if-some [[s m & bindings] (seq bindings)]
    `((second ~monad) (fn [~s] (mlet ~monad ~bindings ~@body)) ~m)
    `((first ~monad) (do ~@body))))

(mlet seqable-monad
  [x [1 2]
   y [3 4]]
  (* x y))
#_=> (3 4 6 8)

(mlet nilable-monad
  [x 1
   y nil
   z 2]
  (* x y z))
#_=> nil
```

This may be an improvement over plain old lambda declaration, but we can do better. You may have detected a smell in the code we had to write : usually, when a `let` binding is used only one time, it can be inlined where it's used. This leads to more concise code and reduces the burden of naming, but `mlet` forbids this. A better syntax would be to have an `mdo` operator, taking a monad definition and a body of expressions, and returning the evaluation of expressions in the context of that monad. Inside the body, we could use a special operator (say, `>>=`) to indicate that the rest of the evaluation must be performed against given monadic value.

```clojure
(mdo seqable-monad
  (* (>>= [1 2]) (>>= [3 4])))
#_=> (3 4 6 8)

(mdo nilable-monad
  (* (>>= 1) (>>= nil) (>>= 2)))
#_=> nil
```

This trick turns out to be very close to delimited continuations, and can be implemented with clonable coroutines as well. `mdo` will be a macro building a coroutine from its body, passing the final result to `unit`, and immediately running it. The coroutine will be suspended on `>>=`. When called, it will build a continuation from the coroutine currently running, pass it to `bind` along with provided monadic value and return the result.
```clojure
(require '[cloroutine.core :refer [cr]])
```

Our dynamic context will hold the coroutine currently running in `*coroutine*`, the monad's bind function in `*bind*`, and the value passed to the continuation in `*result*`.
```clojure
(def ^:dynamic *coroutine*)
(def ^:dynamic *bind*)
(def ^:dynamic *result*)
```

`run` will be used to start/resume the coroutine, binding it to the dynamic context along with the monad's bind function.
```clojure
(defn run [c b]
  (binding [*coroutine* c
            *bind* b] (c)))
```

`fork` will be called on continuation. It binds its argument to the dynamic context, then clone the parent coroutine and resumes it.
```clojure
(defn fork [c b r]
  (binding [*result* r]
    (c run b)))
```

`>>=` wraps the coroutine currently running and the monad's bind function in a continuation, passes it to the bind function along with provided monadic value and returns transformed monadic value.
```clojure
(defn >>= [m]
  (*bind* (partial fork *coroutine* *bind*) m))
```

`=<<` will be the coroutine resume function, returning the argument passed to the continuation from dynamic context.
```clojure
(defn =<< [] *result*)
```

`mdo` builds the coroutine and immediately runs it.
```clojure
(defmacro mdo [monad & body]
  `(run (cr {>>= =<<} ((first ~monad) (do ~@body))) (second ~monad)))
```

As a final example, let's define the state monad and reimplement the parsing game from [haskell documentation](https://wiki.haskell.org/State_Monad).

```clojure
(def state-monad
  [(fn [x] (fn [s] [x s]))
   (fn [f m] (fn [s] (let [[x s] (m s)] ((f x) s))))])

(defn state-eval [m s]
  (first (m s)))

(defn state-get [s]
  [s s])

(defn state-set [s]
  (fn [_] [nil s]))

(defn game [input]
  (mdo state-monad
    (let [[on score] (>>= state-get)]
      (if-some [[x & xs] (seq input)]
        (do (case x
              \a (when on (>>= (state-set [on (inc score)])))
              \b (when on (>>= (state-set [on (dec score)])))
              \c (>>= (state-set [(not on) score])))
            (>>= (game xs))) score))))

(defn play [input]
  (state-eval (game input) [false 0]))

(play "abcaaacbbcabbab")
#_=> 2
```
