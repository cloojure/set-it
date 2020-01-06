(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
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

(def five 5)
(dotest
  (isnt (var? 5)) ; 5 is a value
  (isnt (var? five)) ; auto-deref replaces `five` with `5`
  (is   (var? (var five)))
  (is   (var?    #'five)) ; shortcut for above
  (with-local-vars [x 42]
    (is (var? x))
    (is= 42 (unvar x) (deref x) (var-get x) @x))

  (isnt (atom? 5))
  (is   (atom? (atom 5)))
  (isnt (atom? (agent 5)))

  (isnt (agent? 5))
  (is   (agent? (agent 5)))
  (isnt (agent? (atom 5)))

  (isnt (ref? 5))
  (is   (ref? (ref 5)))
  (isnt (ref? (agent 5)))
  )

(dotest
  (isnt (spyx (var? five)))
  (is (spyx (var? (var five))))
  (is (spyx (var? #'five))) ; => `(var five)`
  ;(spyx (bound? six))   ; syntax error compiling at line XXXXX;  Unable to resolve symbol: six in this context

  (let [local-five     five
        local-var-five (var five)]
    (is= local-five 5)
    (is (var? local-var-five))
    (is= (var-get local-var-five) 5)
    (is= 5 five)
    (isnt= 5 #'five)

    (is= (var five) #'five local-var-five) ; the var object

    (is= 5 ; the var points to this value
      ; `unvar` will dereference a Var object, returning the value the Var points to
      (unvar #'five)
      (unvar (var five))
      (unvar local-var-five))

    ; When passed any rag except a Var object, `unvar` returns it unchanged
    (is= 5
      (unvar five)
      (unvar 5))

    ; There are many ways to deref a Var object
    (is= 5
      (unvar local-five) ; harmless if passed a regular value like `5`
      (unvar local-var-five) ; here it deref's a Var object
      (unvar (unvar local-var-five))) ; idempotent

    ; These 3 are all equivalent
    (is= 5
      (deref local-var-five)
      (var-get local-var-five)
      @local-var-five))
  )

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


(defmacro var-anon
  [val]
  `(def new-var# ~val))

(dotest
  (nl) (println :-----------------------------------------------------------------------------)
  (println :ex-1)
  (prn (macroexpand '(when true 5)))
  (println :ex-2)
  (prn (macroexpand '(demo.core/mylet wilma
                       (+ wilma 3))))
  (println :ex-2)
  (pretty (macroexpand '(demo.core/mut! wilma
                       (+ wilma 3))))

  (nl))

(dotest
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
    (spyx @counter))

  (nl) (println :agent)
  (let [counter (agent nil)]
    (spyx @counter)
    (set-it counter 0)
    (Thread/sleep 99) (spyx @counter)
    (set-it counter (+ it 1))
    (Thread/sleep 99) (spyx @counter))

  (nl) (println :ref)
  (let [counter (ref nil)]
    (spyx (type counter))
    (spyx @counter)
    (dosync
      (set-it counter 0)
      (Thread/sleep 99)) (spyx @counter)
    (dosync
      (set-it counter (+ it 1)))
    (Thread/sleep 99) (spyx @counter))
  )

;-----------------------------------------------------------------------------
(defmacro set-it-local
  [var-obj & forms]
  `(let [~'it (clojure.core/deref ~var-obj) ; or var-get
         ]
     (clojure.core/var-set ~var-obj ~@forms)))

(dotest-focus
  (nl) (println :var-local)
  (with-local-vars [x 1]
    (is (var? x))
    (spyx @x)
    (set-it-local x 3)
    (spyx (get-it-local x))
    (set-it-local x (* it 5))
    (spyx (get-it-local x))
    (spyx @x)

    (nl) (println :-----------------------------------------------------------------------------)
    (println :ex-3)
    (pretty (macroexpand '(demo.core/mut! wilma
                            (+ wilma 3))))

    (spy :x @x)
    (mut! x 21)
    (is= 21  @x)
    (mut! x (* it 2))
    (is= 42 @x)
    (mut! x (+ it (+ x 7) (/ it 7) (/ x 21)))
    (is= 99 @x)

    ) )













