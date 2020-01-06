(ns demo.core )

(defn atom?
  "Returns true iff arg is a clojure.lang.Atom."
  [arg] (= (type arg) clojure.lang.Atom))
(defn agent?
  "Returns true iff arg is a clojure.lang.Agent."
  [arg] (= (type arg) clojure.lang.Agent))
(defn ref?
  "Returns true iff arg is a clojure.lang.Ref."
  [arg] (= (type arg) clojure.lang.Ref))

(defn unvar
  "When passed a clojure var-object, returns the referenced value (via deref/var-get);
  else returns arg unchanged."
  [value-or-var]
  (if (var? value-or-var)
    (deref value-or-var) ; or var-get
    value-or-var))

(defmacro mylet
  [mysym & forms]
  `(do
     (let [~mysym 5]
       ~@forms)))

(defmacro mut!
  [var-obj-name & forms]
  `(let [local-var-obj#     ~var-obj-name
         ~'it               (clojure.core/deref local-var-obj#)
         ~var-obj-name      ~'it]
     (clojure.core/var-set  local-var-obj#
       (do
         ~@forms))))
