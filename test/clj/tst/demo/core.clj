(ns tst.demo.core
  (:use tupelo.core tupelo.test)
  (:require
    [schema.core :as s]
    ))

(comment
  (let [counter (atom nil)]
    (set-it counter 0)
    (set-it counter (+ it 1))
    (satyr counter (+ counter 1))
    )
  )

(defn atom?
  "Returns true iff the arg is a clojure.lang.Atom."
  [arg] (= (type arg) clojure.lang.Atom))
(defn agent?
  "Returns true iff the arg is a clojure.lang.Agent."
  [arg] (= (type arg) clojure.lang.Agent))
(defn ref?
  "Returns true iff the arg is a clojure.lang.Ref."
  [arg] (= (type arg) clojure.lang.Ref))

(defmacro set-it
  [state & forms]
  `(do
     (spyx (var? ~state))
     (cond
       (atom? ~state) (swap! ~state
                        (fn [~'it]
                          ~@forms))

       (agent? ~state) (send ~state
                         (fn [~'it]
                           ~@forms))

       (ref? ~state) (alter ~state
                       (fn [~'it]
                         ~@forms))

       (var? ~state) (alter-var-root ~state
                       (fn [~'it]
                         ~@forms))

       :else (throw (ex-info "Illegal state arg" {:state ~state
                                                  :type  (type ~state)}))
       )))

(defmacro var-set-dynamic-1
  [var-sym val] (list 'set! var-sym val))
(defmacro set-it-dynamic
  [var-sym & forms]
  `(let [~'it ~var-sym]
     (clojure.core/var-set (var ~var-sym) ~@forms)))
(defmacro get-it-local
  [var-sym] `(clojure.core/var-get ~var-sym ))
(defmacro set-it-local
  [var-obj & forms]
  `(let [~'it (clojure.core/deref ~var-obj)] ; or var-get
     (clojure.core/var-set ~var-obj ~@forms)))

(defn unvar
  "When passed a clojure var-object, returns the referenced value (via deref/var-get);
  else returns arg unchanged."
  [value-or-var]
  (if (var? value-or-var)
    (deref value-or-var) ; or var-get
    value-or-var))

(defmacro var-anon
  [val]
  `(def new-var# ~val))

(dotest
  (def five 5)
  (spyx (var? five))
  (spyx (var? (var five)))
 ;(spyx (bound? six))   ; syntax error compiling at line XXXXX;  Unable to resolve symbol: six in this context
  (let [local-five     five
        local-var-five (var five) ]
    (is= local-five 5)
    (is (var? local-var-five))
    (is= (var-get local-var-five) 5)
    (is= 5 five)
    (isnt= 5 #'five)
    (is= 5 (unvar #'five))
    (is= 5 (unvar (var five)))
    (is= 5 (unvar five))
    (is= 5 (unvar 5))
    )

  (is (atom? (atom nil)))
  (isnt (atom? (agent 0)))
  (throws? (set-it {} 5))

  (nl) (println :var-dynamic)
  (def ^:dynamic fred nil)
  (let [fn2 (fn []
              (spyx (var? fred))
              (spyx (bound? (var fred)))
              (spyx (thread-bound? (var fred)))
              (spyx :in fred)
             ;(set! fred 99)
             ;(var-set (var fred) 99)
              (set-it-dynamic fred 77) ; better than:  (set! fred 99)
              (spyx :out fred)
              (set-it-dynamic fred (+ fred 11)) ; better than:  (set! fred 99)
              (spyx :out fred)
              (set-it-dynamic fred (+ it 11)) ; better than:  (set! fred 99)
              (spyx :out fred)
              )]
    (spyx :base fred)
    (binding [fred 5]
      (spyx :before fred)
      (fn2)
      (spyx :after fred))
    (spyx :base fred))

  (nl) (println :var-local)
  (with-local-vars [x 1]
    (spyx @x)
    (set-it-local x 3)
    (spyx (get-it-local x))
    (set-it-local x (* it 5))
    (spyx (get-it-local x))
    (spyx @x)
    )


  (nl) (println :var-anon)
  (let [vv (var-anon nil)]
    (spyx @vv)
    (set-it vv 0)
    (spyx @vv)
    (set-it vv (+ it 1))
    (spyx @vv))

  (nl) (println :atom)
  (let [counter (atom nil)]
    (spyx @counter)
    (set-it counter 0)
    (spyx @counter)
    (set-it counter (+ it 1))
    (spyx @counter) )

  (nl) (println :agent)
  (let [counter (agent nil)]
    (spyx @counter)
    (set-it counter 0)
    (Thread/sleep 99) (spyx @counter)
    (set-it counter (+ it 1))
    (Thread/sleep 99) (spyx @counter) )

  (nl) (println :ref)
  (let [counter (ref nil)]
    (spyx (type counter))
    (spyx @counter)
    (dosync
      (set-it counter 0)
      (Thread/sleep 99)) (spyx @counter)
    (dosync
      (set-it counter (+ it 1)))
    (Thread/sleep 99) (spyx @counter) )
  )


