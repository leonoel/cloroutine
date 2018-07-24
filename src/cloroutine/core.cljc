(ns cloroutine.core
  (:require
    #?(:clj  [cljs.core.async.impl.ioc-macros :as gcc-ioc]
       :cljs [cljs.core.async.impl.ioc-helpers :as gcc-ioc])
    #?(:clj [clojure.core.async.impl.ioc-macros :as jvm-ioc])
    #?(:clj [clojure.tools.analyzer.jvm :as an-jvm]))
  #?(:cljs (:require-macros [cloroutine.core :refer [safe state!]]))
  #?(:clj (:import (java.util.concurrent.atomic AtomicReferenceArray))))

(defmacro safe [[error success] & failure]
  `(try ~success (catch ~(if (:js-globals &env) :default 'Throwable) ~error ~@failure)))

(def ^:const slot-block 1)
(def ^:const slot-value 2)
(def ^:const slot-frames 4)
(def ^:const slot-exception 5)
(def ^:const slot-count 7)

(defmacro state-holder [size]
  (if (:js-globals &env)
    `(object-array ~size)
    `(AtomicReferenceArray. (int ~size))))

(defmacro state!
  ([array slot]
   (if (:js-globals &env)
     `(aget ~array ~slot)
     `(.get ^AtomicReferenceArray ~array ~slot)))
  ([array slot value]
   (if (:js-globals &env)
     `(do (aset ~array ~slot ~value) nil)
     `(.set ^AtomicReferenceArray ~array ~slot ~value))))

(defn recover [state error]
  #?(:clj
     (if-some [[f & fs] (seq (state! state slot-frames))]
       (do (state! state slot-value error)
           (state! state slot-block f)
           (state! state slot-frames fs)
           :recur)
       (throw error))

     :cljs
     (do (state! state slot-exception error)
         (gcc-ioc/process-exception state)
         :recur)))

(defn ioc-return [state value]
  (state! state slot-block -1)
  (state! state slot-value value))

(defn ioc-cr [init size wake step]
  (let [state (state-holder size)]
    (state! state slot-block init)
    #(do
       (when (identical? state (state! state slot-value))
         (safe [e (state! state slot-value (wake))]
           (state! state slot-value e)
           (state! state slot-block -1)))
       (while (some? (safe [e (step state)] (recover state e))))
       (if (neg? (state! state slot-block))
         (state! state slot-value) %))))

#?(:clj
   (defn index-by [f blocks]
     (reduce
       (fn [idx [blk-id blk]]
         (reduce
           (fn [idx inst]
             (reduce
               (fn [idx sym]
                 (update idx sym (fnil conj #{}) blk-id))
               idx (f inst)))
           idx blk))
       {} blocks)))

#?(:clj
   (defn state-machine [reads-from
                        writes-to
                        emit-instruction
                        terminate-block
                        instruction?
                        machine
                        wake
                        transitions]
     (let [blocks (:blocks machine)
           read-in (index-by #(filter instruction? (reads-from %)) blocks)
           written-in (index-by #(filter instruction? (writes-to %)) blocks)
           slots (->> (concat (keys read-in) (keys written-in))
                      (filter (fn [sym]
                                (or (not= (read-in sym)
                                          (written-in sym))
                                    (-> read-in sym count (> 1)))))
                      (reduce
                        (fn [slots sym]
                          (if (contains? slots sym)
                            slots (assoc slots sym (+ slot-count (count slots))))) {}))
           state-sym (gensym "state_")]
       `(ioc-cr
          ~(:start-block machine)
          ~(+ slot-count (count slots))
          (fn [] ~wake)
          (fn [~state-sym]
            (case (int (state! ~state-sym slot-block))
              ~@(into [] (mapcat (fn [[id blk]]
                                   [id `(let [~@(into []
                                                      (comp
                                                        (mapcat reads-from)
                                                        (filter slots)
                                                        (distinct)
                                                        (mapcat (fn [sym] [sym (list `state! state-sym (slots sym))])))
                                                      blk)
                                              ~@(into [] (mapcat #(emit-instruction % state-sym)) (butlast blk))]
                                          ~@(into []
                                                  (comp
                                                    (mapcat writes-to)
                                                    (filter slots)
                                                    (distinct)
                                                    (map (fn [sym] (list `state! state-sym (slots sym) sym))))
                                                  blk)
                                          ~(terminate-block (last blk) state-sym transitions))])) blocks)
              (throw (state! ~state-sym slot-value))))))))

#?(:clj
   (defmacro cr [[wake & suspend] & body]
     (let [transitions (zipmap (map first suspend) (repeatedly gensym))]
       (list `letfn
             (into []
                   (map (fn [[name & spec]]
                          (->> (if (vector? (first spec))
                                 (list spec) spec)
                               (map (fn [[args & body]]
                                      `([state# block# ~@args]
                                         (state! state# slot-block block#)
                                         (state! state# slot-value state#)
                                         ~@body nil)))
                               (cons (transitions name)))))
                   suspend)
             (if (:js-globals &env)
               (let [transitions (-> transitions
                                     (into (map (fn [[k v]] [(symbol (name k)) v])) transitions)
                                     (assoc :Return `ioc-return))]
                 (state-machine gcc-ioc/reads-from
                                gcc-ioc/writes-to
                                gcc-ioc/emit-instruction
                                gcc-ioc/terminate-block
                                gcc-ioc/instruction?
                                (second (gcc-ioc/parse-to-state-machine body &env transitions))
                                wake transitions))
               (let [transitions (assoc transitions :Return `ioc-return)
                     crossing-env (zipmap (keys &env) (repeatedly (partial gensym "env_")))
                     [_ machine] (binding [an-jvm/run-passes jvm-ioc/run-passes]
                                   (-> (an-jvm/analyze
                                         `(let [~@(if (jvm-ioc/nested-go? &env)
                                                    (mapcat (fn [[l {:keys [tag]}]]
                                                              (jvm-ioc/emit-hinted l tag crossing-env)) &env)
                                                    (mapcat (fn [[l ^clojure.lang.Compiler$LocalBinding lb]]
                                                              (jvm-ioc/emit-hinted l (when (.hasJavaClass lb)
                                                                                       (some-> lb .getJavaClass .getName))
                                                                                   crossing-env)) &env))]
                                            ~@body)
                                         (jvm-ioc/make-env &env crossing-env)
                                         {:passes-opts (merge an-jvm/default-passes-opts
                                                              {:uniquify/uniquify-env        true
                                                               :mark-transitions/transitions transitions})})
                                       (jvm-ioc/parse-to-state-machine transitions)))]
                 `(let [~@(mapcat (fn [[l sym]] [sym `(^:once fn* [] ~(vary-meta l dissoc :tag))]) crossing-env)]
                    ~(state-machine jvm-ioc/reads-from
                                    jvm-ioc/writes-to
                                    jvm-ioc/emit-instruction
                                    jvm-ioc/terminate-block
                                    jvm-ioc/instruction?
                                    machine wake transitions))))))))
