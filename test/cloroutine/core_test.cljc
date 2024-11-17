(ns cloroutine.core-test
  (:require [cloroutine.core :refer [#?(:clj cr)]]
            [cloroutine.impl :refer [safe]]
            [clojure.test :refer [deftest is]])
  #?(:cljs (:require-macros [cloroutine.core :refer [cr]])))

#?(:clj (set! *warn-on-reflection* true))

(def check identity)
(defn nop [])
(def npe nil)

(defn run [c & xs]
  (doseq [x xs]
    (is (= x (c)))))

(def values [nil true false 0 "clj" :clj 'clj {:clj 42} #{"clj"} '[clj] '(clj)])
(defprotocol P (p [_]))

(deftest suite
  (doseq [value values] (run (cr {} value) value))
  (run (cr {} (* 6 7)) 42)
  (run (cr {check nop}
         (* (do (check 6) 6)
            (do (check 7) 7)))
       6 7 42)
  (run (cr {} (if nil :then :else)) :else)
  (run (cr {} (if false :then :else)) :else)
  (run (cr {} (if true :then :else)) :then)
  (run (cr {} (if :ok :then :else)) :then)
  (run (cr {} (case nil :default)) :default)
  (run (cr {} (case nil nil :clause :default)) :clause)
  (run (cr {} (case :key :key :clause :default)) :clause)
  (run (cr {} (case 1 1 :clause :default)) :clause)
  (run (cr {} (safe [_ (try
                         (check (throw #?(:clj (Error. "This is fine.") :cljs "This is fine.")))
                         (catch #?(:clj Exception :cljs js/Error) _ :caught))]
                :thrown)) :thrown)
  (run (cr {check nop} (safe [_ (try (check (throw (ex-info "This is fine." {})))
                                     (finally (check :finally)))] :thrown))
       :finally :thrown)
  (run (cr {check nop} (try 42 (finally (check :finally)))) :finally 42)
  (run (cr {check nop} (try (check (throw (ex-info "This is fine." {})))
                            (catch #?(:clj Exception :cljs js/Error) _ :exception)
                            (catch #?(:clj Throwable :cljs :default) _ :throwable)))
       :exception)
  (run (cr {check nop} (safe [_ (do
                                  (try (npe) (finally (safe [_ (npe)] (check :caught))))
                                  (check :dead-code))] :caught)) :caught :caught)
  (run (cr {} (let [a 6
                    b #(* a %)]
                (b 7))) 42)
  (run (cr {} (let [a 6
                    b (fn [& xs] (apply * a xs))]
                (b 7))) 42)
  (run (cr {} (letfn [(a [] (b))
                      (b [] 1)]
                (a))) 1)
  (run (cr {} ((cr {}))) nil)
  (run (cr {} (is (= 0 0))) true)
  (run (cr {} (let [a 42] (p (reify P (p [_] a))))) 42)
  (run (cr {} (.substring "plop" 2)) "op")
  (run (cr {} '(1 2 3)) '(1 2 3))
  (run (cr {} (loop [a :a b :b n 1]
                (if (pos? n)
                  (recur b a (dec n))
                  [a b]))) [:b :a])
  (run (cr {} (loop [x 0
                     y (inc x)] y)) 1)
  (apply run (cr {check nop}
               (loop [x 0]
                 (check x)
                 (if (< x 100)
                   (recur (inc x)))))
         (range 100))
  (apply run (cr {check nop} (dotimes [i 10] (check i)) 10) (range 11))
  (run (cr {check nop}
         (let [[x y] [1 2]] (+ x y))) 3)
  (run (cr {check nop}
         (let [{:keys [x y] x2 :x y2 :y :as foo} {:x 1 :y 2}]
           (check x)
           (check (and foo y x2 y2 foo))
           (+ x y))) 1 {:x 1 :y 2} 3)
  (run (cr {check nop}
         {:a (do (check 1) 1)
          :b (do (check 2) 2)})
       1 2 {:a 1 :b 2})
  (run (cr {check nop}
         #{(do (check 1) 1)
           (do (check 2) 2)})
       1 2 #{1 2})
  (run (cr {check nop}
         [(do (check 1) 1) (do (check 2) 2)])
       1 2 [1 2])
  (run (cr {} (:foo {:foo :bar})) :bar)
  (run (cr {} ([1 2] 1)) 2)
  (run (cr {check nop}
         (loop []
           (when-let [x 10]
             (check (vec (for [i (range x)] i)))
             (if-not x (recur))))) (range 10) nil)
  (let [let* :foo]
    (run (cr {} (let* [x 3] x)) 3))
  (run (cr {} (loop [x 0]
                (case (int x)
                  0 (recur (inc x))
                  1 42))) 42)
  (run (cr {} (set! #?(:clj  (.-gridx (java.awt.GridBagConstraints.))
                       :cljs (.-state (volatile! nil))) 42)) 42)
  (run (cr {} ((comp) 42)) 42)
      (is (= (foo) 42)))
  #?(:cljs (run (cr {} (js->clj #js{:bar 1})) {"bar" 1}))
  #?(:cljs (run (cr {} (js->clj ((fn [] #js{:bar 1})))) {"bar" 1}))
  #?(:cljs (run (cr {} (js->clj #js[1 2 3])) [1 2 3]))
  #?(:cljs (run (cr {} (js->clj ((fn [] #js[1 2 3])))) [1 2 3]))
  )