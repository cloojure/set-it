(ns demo.core
  (:use tupelo.core))

(defn atom?
  "Returns true iff arg is a clojure.lang.Atom."
  [arg] (= (type arg) clojure.lang.Atom))
(defn agent?
  "Returns true iff arg is a clojure.lang.Agent."
  [arg] (= (type arg) clojure.lang.Agent))
(defn ref?
  "Returns true iff arg is a clojure.lang.Ref."
  [arg] (= (type arg) clojure.lang.Ref))

(defn ^:no-doc let-tlv-impl-preproc
  [bindings-user] ; e.g. [a 1 b 2]
  (let [syms-user       (mapv first (partition 2 bindings-user)) ; e.g. [a b]
        syms-gen        (mapv #(gensym %) syms-user) ; e.g. [a24120 b24121]
        bindings-middle (vec (interleave syms-gen syms-user)) ; e.g.  [a24120 a b24121 b]
        bindings-tlv    (vec (interleave syms-user syms-gen))] ; e.g.  [a a24120 b b24121]
    (vals->map syms-user syms-gen bindings-middle bindings-tlv)))

(defn ^:no-doc let-tlv-impl
  [bindings-user  ; e.g. [a 1 b 2]
   forms] ; e.g. [ (prt @a) (prt @b) ]
  (let [preproc-info    (let-tlv-impl-preproc bindings-user)
        bindings-middle (:bindings-middle preproc-info)
        bindings-tlv    (:bindings-tlv preproc-info)]
    `(let ~bindings-user
       (let ~bindings-middle
         (with-local-vars ~bindings-tlv
           ~@forms)))))

(defmacro let-tlv
  [bindings & forms]
  (let-tlv-impl bindings forms)) ; pass `forms` as a seq, not using `apply`

(defmacro tlv-get
  [var-sym] `(clojure.core/var-get ~var-sym ))

(defmacro tlv-set-it
  [ltv-sym & forms]
  `(let [local-var-obj#   ~ltv-sym
         ~'it             (clojure.core/deref local-var-obj#)
      ;  ~ltv-sym         ~'it ; use of target var disabled
        ]
     (clojure.core/var-set local-var-obj#
       (do
         ~@forms))) )

(defmacro set-it
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

(defmacro mylet
  [mysym & forms]
  `(do
     (let [~mysym 5]
       ~@forms)))

(defmacro var-anon
  [val]
  `(def new-var# ~val))

(defn unvar
  "When passed a clojure var-object, returns the referenced value (via deref/var-get);
  else returns arg unchanged. Idempotent to multiple calls."
  [value-or-var]
  (if (var? value-or-var)
    (deref value-or-var) ; or var-get
    value-or-var))























