# Future-based asynchronous processes, aka async/await

In this guide, we'll implement async/await syntax on top of `java.util.concurrent.CompletableFuture`. Unlike clojure's `future`, java's `CompletableFuture` allows nonblocking composition at the price of additional syntax complexity.

This syntax burden is familiar to javascript developers and leads to the famous callback-hell when improperly controlled. The async/await syntax, a popular answer to this problem, allows synchronous-looking code to be converted into an asynchronous process.

As we'll see, coroutines can easily emulate async/await, and this technique can be transposed to any callback-based asynchronous primitive, including javascript promises in clojurescript where blocking is not an option.

```clojure
(require '[cloroutine.core :refer [cr]])
(import '(java.util.concurrent CompletableFuture))
```

First, let's define our break points and their associated thread-local context. The asynchronous processes we'll build with `async` blocks are lightweight threads scheduled in user space, we'll refer to them as `*fiber*`s and model them with `java.util.function.BiConsumer`. The `await` function will suspend the fiber and register it on given `CompletableFuture`. When the future resolves, a `*value*` or an `*error*` becomes available to be consumed by `thunk`, the resume function associated with `await`.

```clojure
(def ^:dynamic *fiber*)
(def ^:dynamic *value*)
(def ^:dynamic *error*)
(defn await [cf] (.whenComplete ^CompletableFuture cf *fiber*))
(defn thunk [] (if-some [e *error*] (throw e) *value*))
```

We can now define async blocks as a macro wrapping its body in a coroutine, passing its result or error to a fresh future. The fresh coroutine is then wrapped in a fiber and immediately started, and the future is returned.

```clojure
(defmacro async [& body]
  `(let [cf# (CompletableFuture.)
         cr# (cr {await thunk}
               (try (.complete cf# (do ~@body))
                    (catch Throwable e#
                      (.completeExceptionally cf# e#))))]
     (binding [*fiber* (reify java.util.function.BiConsumer
                         (accept [f# v# e#]
                           (binding [*fiber* f#
                                     *value* v#
                                     *error* e#]
                             (cr#))))]
       (cr#)) cf#))
```

We can now define `async` blocks and use `await` inside to *park* on a future result.

```clojure
(def six (async 6))                                                      ;; a future of 6
(def seven (async (inc (await six))))                                    ;; a future of 7
(def failed (async (throw (ex-info "this is fine." {}))))                ;; a failed future
(def recovered (async (try (await failed) (catch Exception e :failed)))) ;; a future of :failed
```
