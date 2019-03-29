# Delimited continuations

In this guide, we'll implement a clojure version of scheme's `shift`/`reset` control flow operators. The idea is to be able to define a block of code inside which we could arbitrarily break current execution state, deferring evaluation of the rest of the block, and have this continuation reified as a plain function.

If you're familiar with scheme's operators, ours will be slightly different :
* Instead of taking a binding and a body, our `shift` will take a function along with optional arguments and call that function with the continuation as first argument, followed by optional arguments. This style is just more idiomatic clojure, althought there wouldn't be any technical limitation to do it the scheme way.
* Unlike scheme, our host platform provides very limited stack manipulation features so we can't shift/reset across stack frames. That's why our solution relies on static, macro-based, lexical techniques, preventing us to `shift` outside of the synchronous boundaries of a `reset` block.

What we want is to write code like this :
```clojure
(* 2 (reset (+ 1 (shift function with some args))))
```

And be it equivalent to this :
```clojure
(* 2 (function #(+ 1 %) with some args))
```

`reset` will be a macro building a coroutine from its body, and immediately running it. The coroutine will be suspended on `shift`. When called, it must capture the reference of the coroutine currently running, wrap it in a continuation, pass it to given function and return the result. Because the continuation is exposed to the wild and can be called an arbitrary number of times in an arbitrary number of threads, it must first clone the captured coroutine, then provide given argument to the dynamic context so that the resume function can access it, and finally resume the freshly cloned coroutine.
```clojure
(require '[cloroutine.core :refer [cr]])
```

Our dynamic context will hold the coroutine currently running in `*coroutine*`, and the argument passed to the continuation in `*result*`.
```clojure
(def ^:dynamic *coroutine*)
(def ^:dynamic *result*)
```

`run` will be used to start/resume the coroutine, binding it to the dynamic context.
```clojure
(defn run [c]
  (binding [*coroutine* c] (c)))
```

`fork` will be called on continuation. It binds its argument to the dynamic context, then clones the parent coroutine and resumes it.
```clojure
(defn fork [c x]
  (binding [*result* x] (c run)))
```

`shift` captures the coroutine currently running, builds a continuation from it, passes it to given function and returns the result.
```clojure
(defn shift [f & args]
  (apply f (partial fork *coroutine*) args))
```

`thunk` will provide the continuation argument on coroutine resume, defining the result of `shift` in the `reset` body.
```clojure
(defn thunk [] *result*)
```

`reset` builds the coroutine and immediately runs it.
```clojure
(defmacro reset [& body]
  `(run (cr {shift thunk} ~@body)))
```

And we're done.
```clojure
(reset (* 2 (shift map (range 3))))
#_=> (0 2 4)
```
