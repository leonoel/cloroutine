(ns cloroutine.core (:require [cloroutine.impl :as i]))

(defmacro
  ^{:arglists '([breaks & body]) :doc "
Builds a coroutine evaluating body (in an implicit do), suspending evaluation at
call sites defined in the breaks map. The breaks map associates suspend vars to
resume vars, both given as fully-qualified symbols.

A coroutine is a stateful zero-arity function. Calling a coroutine in initial or
suspended state starts or resumes evaluation and returns the result of the next
encountered suspend var call or final body expression.

After calling a suspend var, the coroutine is in suspended state. A subsequent
coroutine call will resume body evaluation, the result of the suspend var call
being substituted with the result of calling its associated resume var with zero
argument.

After reaching final body expression, or getting an uncaught exception, the
coroutine is in terminated state. Subsequent calls have undefined behavior.

The state of a coroutine is not synchronized, each call to a suspend var is
expected to happen-before its associated coroutine resume call.

Suspend and resume vars are guaranteed to be called synchronously in the thread
calling the coroutine. For this reason, the cr macro will ignore breaking vars
in code able to escape synchronous execution context. This includes function
bodies and custom type methods.

Calling the non-zero arity of a coroutine will clone the coroutine and pass the
copy to the function provided as first argument, and return the result. Other
arguments are passed to the function as-is.
"} cr [breaks & body]
  (i/compile (gensym "cr") breaks &env (cons `do body)))
