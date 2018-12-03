(ns cloroutine.core-test
  (:require [cloroutine.core :refer [cr]]
            [cloroutine.impl :refer [safe]]
            [clojure.test :refer [deftest is]]
            #?(:cljs [goog.async.nextTick]))
  #?(:cljs (:require-macros
             [cloroutine.core-test :refer [bk]])))

#?(:clj (set! *warn-on-reflection* true))

(def check identity)
(defn nop [])

(defmacro bk [& body]
  `(cr {check nop} ~@body))

(defn run [c & xs]
  (doseq [x xs]
    (is (= x (c)))))

(def values [nil true false 0 "clj" :clj 'clj {:clj 42} #{"clj"} '[clj] '(clj)])
(defprotocol P (p [_]))

(deftest suite
  (doseq [value values] (run (bk value) value))
  (run (bk (* 6 7)) 42)
  (run (bk (* (do (check 6) 6)
              (do (check 7) 7)))
       6 7 42)
  (run (bk (if nil :then :else)) :else)
  (run (bk (if false :then :else)) :else)
  (run (bk (if true :then :else)) :then)
  (run (bk (if :ok :then :else)) :then)
  (run (bk (case nil :default)) :default)
  (run (bk (case nil nil :clause :default)) :clause)
  (run (bk (case :key :key :clause :default)) :clause)
  (run (bk (case 1 1 :clause :default)) :clause)
  (run (bk (safe [_ (try
                      (check (throw #?(:clj (Error. "This is fine.") :cljs "This is fine.")))
                      (catch #?(:clj Exception :cljs js/Error) _ :caught))]
             :thrown)) :thrown)
  (run (bk (safe [_ (try (check (throw (ex-info "This is fine." {})))
                         (finally (check :finally)))] :thrown))
       :finally :thrown)
  (run (bk (try 42 (finally (check :finally)))) :finally 42)
  (run (bk (try (check (throw (ex-info "This is fine." {})))
                (catch #?(:clj Exception :cljs js/Error) _ :exception)
                (catch #?(:clj Throwable :cljs :default) _ :throwable)))
       :exception)
  (run (bk (safe [_ (let [npe! nil]
                      (try (npe!) (finally (safe [_ (npe!)] (check :caught))))
                      (check :dead-code))] :caught)) :caught :caught)
  (run (bk (let [a 6
                 b #(* a %)]
             (b 7))) 42)
  (run (bk (let [a 6
                 b (fn [& xs] (apply * a xs))]
             (b 7))) 42)
  (run (bk (letfn [(a [] (b))
                   (b [] 1)]
             (a))) 1)
  (run (bk ((bk))) nil)
  (run (bk (is (= 0 0))) true)
  (run (bk (let [a 42] (p (reify P (p [_] a))))) 42)
  (run (bk (.substring "plop" 2)) "op")
  (run (bk '(1 2 3)) '(1 2 3))
  (run (bk (loop [a :a b :b n 1]
             (if (pos? n)
               (recur b a (dec n))
               [a b]))) [:b :a])
  (run (bk (loop [x 0
                  y (inc x)] y)) 1)
  (apply run (bk (loop [x 0]
                   (check x)
                   (if (< x 100)
                     (recur (inc x)))))
         (range 100))
  (apply run (bk (dotimes [i 10] (check i)) 10) (range 11))
  (run (bk (let [[x y] [1 2]] (+ x y))) 3)
  (run (bk (let [{:keys [x y] x2 :x y2 :y :as foo} {:x 1 :y 2}]
             (check x)
             (check (and foo y x2 y2 foo))
             (+ x y))) 1 {:x 1 :y 2} 3)
  (run (bk {:a (do (check 1) 1)
            :b (do (check 2) 2)})
       1 2 {:a 1 :b 2})
  (run (bk #{(do (check 1) 1)
             (do (check 2) 2)})
       1 2 #{1 2})
  (run (bk [(do (check 1) 1) (do (check 2) 2)])
       1 2 [1 2])
  (run (bk (:foo {:foo :bar})) :bar)
  (run (bk ([1 2] 1)) 2)
  (run (bk (loop []
             (when-let [x 10]
               (check (vec (for [i (range x)] i)))
               (if-not x (recur))))) (range 10) nil)
  (let [let* :foo]
    (run (bk (let* [x 3] x)) 3))
  (run (bk (loop [x 0]
             (case (int x)
               0 (recur (inc x))
               1 42))) 42)
  (run (bk (set! #?(:clj (.-gridx (java.awt.GridBagConstraints.))
                    :cljs (.-state (volatile! nil))) 42)) 42)
  )




