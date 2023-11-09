;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.ordered-contexts
  "Provides the implementation of formal contexts with a given order on objects and attributes."
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all])
  (:import [conexp.fca.contexts Formal-Context]))

;;;

(defn- sort-things
  [things order-on-things]
  (cond
    (sequential? order-on-things)
    (let [order-on-things (filter things order-on-things)]
      (concat order-on-things
              (difference things (set order-on-things)))),
    (fn? order-on-things)
    (vec (sort order-on-things things)),
    :otherwise
    (illegal-argument "Ordering must be either a sequence or a function.")))

(deftype Ordered-Context [objects attributes incidence order-on-objects order-on-attributes]
  Object
  (equals [this other]
    (and (instance? Ordered-Context other)
         (= (set objects) (set (.objects ^Ordered-Context other)))
         (= (set attributes) (set (.attributes ^Ordered-Context other)))
         (let [other-incidence (.incidence ^Ordered-Context other)]
           (forall [g objects, m attributes]
                   (<=> (incidence [g m])
                        (other-incidence [g m]))))
         (= (sort-things (set objects) order-on-objects)
            (sort-things (set (.objects ^Ordered-Context other))
                         (.order-on-objects ^Ordered-Context other)))
         (= (sort-things (set attributes) order-on-attributes)
            (sort-things (set (.attributes ^Ordered-Context other))
                         (.order-on-attributes ^Ordered-Context other)))))
  (hashCode [this]
    (hash-combine-hash Ordered-Context 
                       objects 
                       attributes 
                       incidence 
                       order-on-objects 
                       order-on-attributes))
  ;;
  Context
  (objects [this] objects)
  (attributes [this] attributes)
  (incidence [this] incidence))

(defn order-on-objects
  "Returns order on the objects of the context."
  [^Ordered-Context context]
  (.order-on-objects context))

(defn order-on-attributes
  "Returns order on the attributes of the context."
  [^Ordered-Context context]
  (.order-on-attributes context))

(defn ordered-context?
  "Returns true iff thing is an ordered context."
  [thing]
  (instance? Ordered-Context thing))

(defmethod print-method Ordered-Context [ctx out]
  (.write ^java.io.Writer out
          ^String (context-to-string ctx
                                     (order-on-objects ctx)
                                     (order-on-attributes ctx))))

;; Constructors

(defn make-ordered-context-nc
  "Creates an ordered context from the given arguments, without any checks."
  ([objects attributes incidence]
   (make-ordered-context-nc objects attributes incidence objects attributes))
  ([objects attributes incidence order-on-objects order-on-attributes]
   (Ordered-Context. (set objects)
                     (set attributes)
                     incidence
                     order-on-objects
                     order-on-attributes)))

(defmulti make-ordered-context
  "Standard constructor for ordered contexts. Takes a sequence of objects, 
  a sequence of attributes and either a set of pairs or a function of two arguments being 
  true iff its arguments are incident.
  The order on the objects and attributes is either given with the order on the sequences,
  or given separately as a sequence or function by order-on-objects 
  and order-on-attributes.
  The standard constructor can also build an ordered context from a Formal-Context 
  and given order-on-objects and order-on-attributes."
  {:arglist '([objects attributes incidence]
              [context order-on-objects order-on-attributes]
              [objects attributes incidence order-on-objects order-on-attributes])}
  (fn [& args]
    (vec (map clojure-type args))))

(defmethod make-ordered-context [clojure-seq clojure-seq clojure-coll]
  [objects attributes incidence]
  (make-ordered-context objects attributes incidence objects attributes))

(defmethod make-ordered-context [clojure-seq clojure-seq clojure-fn]
  [objects attributes incidence]
  (make-ordered-context objects attributes incidence objects attributes))

(defmethod make-ordered-context [clojure-coll clojure-coll Object clojure-seq clojure-seq]
  [objects attributes incidence order-on-objects order-on-attributes]
  (let [formal-context (make-context objects attributes incidence)]
    (make-ordered-context formal-context order-on-objects order-on-attributes)))

(defmethod make-ordered-context [clojure-coll clojure-coll Object clojure-fn clojure-fn]
  [objects attributes incidence order-on-objects order-on-attributes]
  (let [formal-context (make-context objects attributes incidence)]
    (make-ordered-context formal-context order-on-objects order-on-attributes)))

(defmethod make-ordered-context [Formal-Context clojure-seq clojure-seq]
  [ctx order-on-objects order-on-attributes]
  (assert (subset? (objects ctx) (set order-on-objects))
          "All objects need to be considered in the order-on-objects.")
  (assert (subset? (attributes ctx) (set order-on-attributes))
          "All attributes need to be considered in the order-on-attributes.")
  (Ordered-Context. (objects ctx)
                    (attributes ctx)
                    (incidence ctx)
                    order-on-objects
                    order-on-attributes))

(defmethod make-ordered-context [Formal-Context clojure-fn clojure-fn]
  [ctx order-on-objects order-on-attributes]
  (let [order-fn-on-objects (fn [x y]
                              (order-on-objects x y))
        order-fn-on-attributes (fn [x y]
                                 (order-on-attributes x y))]
    (Ordered-Context. (objects ctx)
                      (attributes ctx)
                      (incidence ctx)
                      order-fn-on-objects
                      order-fn-on-attributes)))

(defmethod make-ordered-context :default 
  [& args]
  (illegal-argument "The arguments " args 
                    " are not valid for an ordered context."))

;;

(defn make-ordered-context-from-matrix
  "With given objects, attributes and an incidence matrix, constructs the 
  corresponding context. G and M may also be numbers where they represent (range G)
  and (range M) respectively."
  ([objects attributes bits]
   (make-ordered-context-from-matrix objects attributes bits objects attributes))
  ([objects attributes bits order-on-objects order-on-attributes]
   (let [context (make-context-from-matrix objects attributes bits)]
     (make-ordered-context context
                           order-on-objects 
                           order-on-attributes))))

(defn rand-ordered-context
  "With given objects and attributes, make a random ordered context."
  [objects attributes fill-rate]
  (let [context (rand-context objects attributes fill-rate)]
    (make-ordered-context context objects attributes)))

;;

(defn rename-ordered-objects
  "Rename objects in context by given function old-to-new."
  [context old-to-new]
  (let [new-context (rename-objects (make-context (objects context)
                                                  (attributes context)
                                                  (incidence context))
                                    old-to-new)
        new-object-order (map old-to-new 
                              (sort-things (set (objects context))
                                           (order-on-objects context)))
        attribute-order (sort-things (set (attributes context))
                                     (order-on-attributes context))]
    (make-ordered-context new-context 
                          new-object-order
                          attribute-order)))
