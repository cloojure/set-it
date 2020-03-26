(ns demo.core
  (:use tupelo.core)
  (:import [com.sun.org.apache.xalan.internal.xsltc.compiler When]))

;-----------------------------------------------------------------------------
(defn atom?
  "Returns true iff arg is a clojure.lang.Atom."
  [arg] (= (type arg) clojure.lang.Atom))
(defn agent?
  "Returns true iff arg is a clojure.lang.Agent."
  [arg] (= (type arg) clojure.lang.Agent))
(defn ref?
  "Returns true iff arg is a clojure.lang.Ref."
  [arg] (= (type arg) clojure.lang.Ref))

;-----------------------------------------------------------------------------
(defmacro var-anon
  [val]
  `(def new-var# ~val))

(defn unvar
  "When passed a clojure var-object, returns the referenced value (via deref/var-get).
  Otherwise, returns arg unchanged. Idempotent to multiple calls."
  [value-or-var]
  (if (var? value-or-var)
    (deref value-or-var) ; or var-get
    value-or-var))

;-----------------------------------------------------------------------------
(defmacro set-it
  "Changes the value of a Var, Atom, Agent, or Ref using either `set` or `update` style, where the
   current value is available via the placeholder symbol `it`:

        (dotest
          (let [x (atom 5)]
            (is= @x 5)              ; initial value
            (set-it x 6)            ; assign new value
            (is= (deref x) 6)       ; can use `@` or `deref` to access value
            (set-it x (* it 7))     ; update old value using `it` placeholder symbol
            (is= @x 42)))           ; verify expected result
 "
  [state & forms]
  `(do
     ;(spyx (var? ~state))
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

(defmacro set-it-dynamic
  [var-sym & forms]
  `(let [~'it ~var-sym]
     (clojure.core/var-set (var ~var-sym) ~@forms)))
(comment  ; alternate implementation
  (defmacro var-set-dynamic-1
    [var-sym val] (list 'set! var-sym val)))

(defmacro set-it-in
  [mappy path & forms]
  `(let [val-orig#     (fetch-in ~mappy ~path)
         delta-fn#     (fn [~'it] ~@forms)
         val-new#      (delta-fn# val-orig#)
         mappy-result# (assoc-in ~mappy ~path val-new#)]
     mappy-result#))

;-----------------------------------------------------------------------------
(defn ^:no-doc let-tlv-impl-preproc
  [bindings-user] ; e.g. [a 1 b 2]
  (let [syms-user       (mapv first (partition 2 bindings-user)) ; e.g. [a b]
        syms-gen        (mapv #(gensym %) syms-user) ; e.g. [a24120 b24121]
        bindings-middle (vec (interleave syms-gen syms-user)) ; e.g.  [a24120 a b24121 b]
        bindings-tlv    (vec (interleave syms-user syms-gen))] ; e.g.  [a a24120 b b24121]
    (vals->map syms-user syms-gen bindings-middle bindings-tlv)))

(defn ^:no-doc let-tlv-impl
  [bindings-user ; e.g. [a 1 b 2]
   forms] ; e.g. [ (prt @a) (prt @b) ]
  (let [preproc-info    (let-tlv-impl-preproc bindings-user)
        bindings-middle (:bindings-middle preproc-info)
        bindings-tlv    (:bindings-tlv preproc-info)]
    `(let ~bindings-user
       (let ~bindings-middle
         (with-local-vars ~bindings-tlv
           ~@forms)))))

(defmacro tlv-let
  "Creates Thread-Local-Var (TLVs) in a `let` form"
  [bindings & forms]
  (let-tlv-impl bindings forms)) ; pass `forms` as a seq, not using `apply`

(defmacro tlv-get
  "Returns value of a Thread-Local-Var (TLVs)"
  [var-sym] `(clojure.core/var-get ~var-sym))

(defmacro tlv-set-it
  "Modifies the value of a Thread-Local-Var (TLVs) using the `it` placeholder"
  [ltv-sym & forms]
  `(let [local-var-obj# ~ltv-sym
         ~'it (clojure.core/deref local-var-obj#)
         ;  ~ltv-sym         ~'it ; use of target var disabled
         ]
     (clojure.core/var-set local-var-obj#
       (do
         ~@forms))))



