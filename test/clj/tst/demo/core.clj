(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test))

(def five 5)

(dotest
  (isnt (var? five))
  (is (var? (var five)))
  (is (var? #'five)) ; => `(var five)`
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

    ; When passed any arg except a Var object, `unvar` returns it unchanged
    (is= 5
      (unvar five)
      (unvar 5))

    ; There are many ways to deref a Var object
    (is= 5
      (unvar local-five) ; harmless if passed a regular value like `5`
      (unvar local-var-five) ; here it deref's a Var object
      (unvar (unvar local-var-five))) ; idempotent

    ; These are all equivalent
    (is= 5
      (unvar local-var-five)
      (var-get local-var-five)
      (deref local-var-five)
      @local-var-five)))

(dotest
  (isnt (var? 5)) ; 5 is a value
  (isnt (var? five)) ; auto-deref replaces `five` with `5`
  (is (var? (var five)))
  (is (var? #'five)) ; shortcut for above
  (with-local-vars [x 42]
    (is (var? x))
    (is= 42
      (unvar x) ; these all yield the same value
      (deref x)
      (var-get x)
      @x))

  (let [the-value 5
        the-var (var-anon the-value)
        the-atom (atom the-value)
        the-agent (agent the-value)
        the-ref (ref the-value)
        value? (fn [arg] (= 5 arg)) ]

    (is= the-value
      @the-var
      @the-atom
      @the-agent
      @the-ref )

    (is (var? the-var))
    (is (atom? the-atom))
    (is (agent? the-agent))
    (is (ref? the-ref))

    (is   (value? the-value))
    (isnt (var? the-value))
    (isnt (atom? the-value))
    (isnt (agent? the-value))
    (isnt (ref? the-value))

    (isnt (value? the-var))
    (is   (var? the-var))
    (isnt (atom? the-var))
    (isnt (agent? the-var))
    (isnt (ref? the-var))

    (isnt (value? the-atom))
    (isnt (var? the-atom))
    (is   (atom? the-atom))
    (isnt (agent? the-atom))
    (isnt (ref? the-atom))

    (isnt (value? the-agent))
    (isnt (var? the-agent))
    (isnt (atom? the-agent))
    (is   (agent? the-agent))
    (isnt (ref? the-agent))

    (isnt (value? the-ref))
    (isnt (var? the-ref))
    (isnt (atom? the-ref))
    (isnt (agent? the-ref))
    (is   (ref? the-ref))))

;---------------------------------------------------------------------------------------------------
(dotest
  (throws? (set-it {} 5))

  (let [vv (var-anon nil)]
    (is= nil @vv)
    (set-it vv 0)
    (is= 0 @vv)
    (set-it vv (inc it))
    (is= 1 @vv))

  (let [counter (atom nil)]
    (is= nil @counter)
    (set-it counter 0)
    (is= 0 @counter)
    (set-it counter (inc it))
    (is= 1 @counter))

  (let [counter (agent nil)]
    (is= nil @counter)
    (set-it counter 0)
    (Thread/sleep 234)
    (is= 0 @counter)
    (set-it counter (inc it))
    (Thread/sleep 234)
    (is= 1 @counter))

  (let [counter (ref nil)]
    (is= clojure.lang.Ref (type counter))
    (is= nil @counter)
    (dosync
      (set-it counter 0)
      (is= 0 @counter))
    (dosync
      (set-it counter (inc it)))
    (is= 1 @counter)))

(dotest
  (let [x (atom 5)]
    (is= @x 5)
    (set-it x 6)
    (is= (deref x) 6)
    (set-it x (* it 7))
    (is= @x 42)))

;---------------------------------------------------------------------------------------------------
; Parents are regular Vars.  Kids are dynamic Vars.
(def barney)
(def betty "Hi")
(def ^:dynamic pebbles)
(def ^:dynamic bambam "Bam!")

(dotest
  ; only vars supplied with an initial value are `bound`
  (is (bound? #'betty))
  (is (bound? #'bambam))
  (isnt (bound? #'barney))
  (isnt (bound? #'pebbles))

  ; none of them are `thread-bound` since we haven't used the `binding` form
  (isnt (thread-bound? #'barney))
  (isnt (thread-bound? #'betty))
  (isnt (thread-bound? #'pebbles))
  (isnt (thread-bound? #'bambam))

  ; Predicate `thread-bound?` returns true iff:
  ;   (and <var is dynamic>
  ;        <within a `binding` scope> )
  (binding [pebbles 3 ; only dynamic vars can be used in a `binding` form
            bambam  4]
    (is (bound? #'pebbles)) ; pebbles is now bound (to 3)
    (is (bound? #'bambam))  ; bambam is still bound, but to a new value 4
    (is (thread-bound? #'pebbles)) ; both are thread-bound since we...
    (is (thread-bound? #'bambam)) ;  ...are in the `binding` form
    (is= 3 pebbles) ; both values can be accessed thru the global symbol...
    (is= 4 bambam))) ;  ...and we see the dynamic value present

(def ^:dynamic fred nil)
(dotest
  (let [dyny-fn (fn []
                  (is= 5 fred)
                  (isnt (var? fred)) ; fred resolves to 5, not a Var object
                  (is (bound? (var fred))) ; the Var object has both a root binding...
                  (is (thread-bound? (var fred))) ; ...and a thread binding

                  ; 2 old ways to set a dynamic var
                  (set! fred 22) ; use global symbol
                  (is= 22 fred)
                  (var-set (var fred) 33) ; get the Var obj from the global symbol
                  (is= 33 fred)

                  ; easier, new way to set a dynamic var
                  (set-it-dynamic fred 77) ; set to a constant
                  (is= 77 fred)
                  (set-it-dynamic fred (+ fred 11)) ; update using dynamic global value
                  (is= 88 fred)
                  (set-it-dynamic fred (+ it 11)) ; update using `it` placeholder symbol
                  (is= 99 fred)
                  ; modified dynamic value is returned
                  )
        ]
    (is= nil fred) ; root value
    (binding [fred 5] ; set fred to 5 in a dynamic frame
      (is= 5 fred)
      (dyny-fn) ; function both reads dynamic value 5, then updates it to 99
      (is= 99 fred))
    (is= nil fred))) ; outside of `binding`, we are back to the original root value

;-----------------------------------------------------------------------------
; TLVs => Thread Local Vars
(dotest
  (let [tlv-info (let-tlv-impl-preproc (quote [a 1 b 2]))]
    (with-map-vals tlv-info [syms-user syms-gen bindings-middle bindings-tlv]
      (is= syms-user (quote [a b]))
      (is (wild-match? (quote [:* a :* b]) bindings-middle))
      (is (wild-match? (quote [a :* b :*]) bindings-tlv))))

  ; Macro `let-tlv` will expand like this:
  (let [a 1
        b (+ a 2)]
    (let [a0 a
          b0 b]
      (with-local-vars [a a0
                        b b0]
        (is= 1 @a)
        (is= 3 @b))))

  (tlv-let [x 5
            y (+ x 2)] ; can refer to previous locals, unlike `with-local-vars`
    (is= clojure.lang.Var
      (type x)
      (type y))
    (is= 5 (tlv-get x)) ; or @x
    (is= 7 (tlv-get y)) ; or @y
    (tlv-set-it x (inc it)) ; no `tlv-get` required on target var if use pronoun `it`
    (is= 6 (tlv-get x))
    (tlv-set-it y (* it (tlv-get x))) ; can use `it` for target; `tlv-get` required for other TLVs
    (is= 42 @y)

    ; #todo need example with multiple threads to demonstrate thread-local nature
    ; #todo what happens when you pass a TLV to a function?  via a Future?
    ))

(dotest
  (let [mult-int (fn [a b]
                   (assert (and (int-pos? a) (int-pos? b)))
                   (tlv-let [cum 0
                             cntr a]
                     (while (pos? (tlv-get cntr))
                       ;(spyx [@cum @cntr])  ; can use reader macro `@` to deref
                       (tlv-set-it cum (+ it b))
                       (tlv-set-it cntr (dec it)))
                     (tlv-get cum))) ; can use `tlv-get` or `deref` or `@`
        ]
    (is= 12 (mult-int 4 3))))

;-----------------------------------------------------------------------------
(dotest
  (let [mm {:a {:b {:c 3}}}
        m2 (set-it-in mm [:a :b :c] 666) ; ignore old value
        m3 (set-it-in mm [:a :b :c] (* 6 (+ 4 it)))] ; modify old value
    (is= m2 {:a {:b {:c 666}}})
    (is= m3 {:a {:b {:c 42}}})
    (throws? (set-it-in mm [:a :b :zzz] -1)))) ; throws for invalid path



























