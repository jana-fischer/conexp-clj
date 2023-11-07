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
    (hash-combine-hash Ordered-Context objects attributes incidence order-on-objects order-on-attributes))
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
  and order-on-attributes."
  {:arglist '([objects attributes incidence]
              [objects attributes incidence order-on-objects order-on-attributes])}
  (fn [& args]
    (vec (map clojure-type args))))

(defmethod make-ordered-context [clojure-seq clojure-seq clojure-coll]
  [objects attributes incidence]
  (make-ordered-context objects attributes incidence objects attributes))

(defmethod make-ordered-context [clojure-seq clojure-seq clojure-fn]
  [objects attributes incidence]
  (make-ordered-context objects attributes incidence objects attributes))

(defmethod make-ordered-context [clojure-coll clojure-coll clojure-coll clojure-seq clojure-seq]
  [objects attributes incidence order-on-objects order-on-attributes]
  (assert (subset? objects (set order-on-objects))
          "All objects need to be considered in the order-on-objects.")
  (assert (subset? attributes (set order-on-attributes))
          "All attributes need to be considered in the order-on-attributes.")
  (let [object-set (to-set objects)
        attribute-set (to-set attributes)
        incidence-set (set-of [g m] [[g m] incidence
                                     :when (and (contains? object-set g)
                                                (contains? attribute-set m))])]
    (Ordered-Context. object-set 
                      attribute-set 
                      incidence-set 
                      order-on-objects 
                      order-on-attributes)))

(defmethod make-ordered-context [clojure-coll clojure-coll clojure-fn clojure-seq clojure-seq]
  [objects attributes incidence order-on-objects order-on-attributes]
  (assert (subset? objects (set order-on-objects))
          "All objects need to be considered in the order-on-objects.")
  (assert (subset? attributes (set order-on-attributes))
          "All attributes need to be considered in the order-on-attributes.")
  (Ordered-Context. (set objects)
                    (set attributes)
                    (fn [[g m]]
                      (incidence g m))
                    order-on-objects
                    order-on-attributes))

(defmethod make-ordered-context [clojure-coll clojure-coll clojure-coll clojure-fn clojure-fn]
  [objects attributes incidence order-on-objects order-on-attributes]
  (let [object-set (to-set objects)
        attribute-set (to-set attributes)
        incidence-set (set-of [g m] [[g m] incidence
                                     :when (and (contains? object-set g)
                                                (contains? attribute-set m))])
        order-fn-on-objects (fn [x y]
                              (order-on-objects x y))
        order-fn-on-attributes (fn [x y]
                                 (order-on-attributes x y))]
    (Ordered-Context. object-set
                      attribute-set
                      incidence-set
                      order-fn-on-objects
                      order-fn-on-attributes)))

(defmethod make-ordered-context [clojure-coll clojure-coll clojure-fn clojure-fn clojure-fn]
  [objects attributes incidence order-on-objects order-on-attributes]
  (let [object-set (to-set objects)
        attribute-set (to-set attributes)
        incidence-fn (fn [[g m]]
                       (incidence g m))
        order-fn-on-objects (fn [x y]
                              (order-on-objects x y))
        order-fn-on-attributes (fn [x y]
                                 (order-on-attributes x y))]
    (Ordered-Context. object-set
                      attribute-set
                      incidence-fn
                      order-fn-on-objects
                      order-fn-on-attributes)))

(defmethod make-ordered-context [Formal-Context clojure-seq clojure-seq]
  [ctx order-on-objects order-on-attributes]
  (make-ordered-context (objects ctx)
                        (attributes ctx)
                        (incidence ctx)
                        order-on-objects
                        order-on-attributes))

(defmethod make-ordered-context :default 
  [& args]
  (illegal-argument "The arguments " args 
                    " are not valid for an ordered context."))

(defn make-ordered-context-from-matrix
  "With given objects, attributes and an incidence matrix, constructs the 
  corresponding context. G and M may also be numbers where they represent (range G)
  and (range M) respectively."
  ([objects attributes bits]
   (make-ordered-context-from-matrix objects attributes bits objects attributes))
  ([objects attributes bits order-on-objects order-on-attributes]
   (assert (forall [x bits] (or (= 1 x) (= 0 x)))
           "All entries given must be either 0 or 1.")
   (assert (or (sequential? order-on-objects)
               (fn? order-on-objects))
           "Order on objects must either be a sequence or a function.")
   (assert (or (sequential? order-on-attributes)
               (fn? order-on-attributes))
           "Order on attributes must either be a sequence or a function.")
   (let [objects (ensure-seq objects),
         attributes (ensure-seq attributes),
         object-count (count objects),
         attribute-count (count attributes)]
     (assert (= (* object-count attribute-count) (count bits))
             "Number of objects and attributes does not match the number of entries.")
     (make-ordered-context-nc objects attributes
                              (set-of [a b] [i (range object-count),
                                             j (range attribute-count),
                                             :when (= 1 (nth bits (+ (* attribute-count i) j)))
                                             :let [a (nth objects i),
                                                   b (nth attributes j)]])
                              order-on-objects order-on-attributes))))

(defn rand-ordered-context
  "With given objects and attributes, make a random ordered context."
  [objects attributes fill-rate]
  (when-not (and (number? fill-rate)
                 (<= 0 fill-rate 1))
    (illegal-argument "Fill-rate must be a number between 0 and 1."))
  (let [object-set (to-set objects)
        attribute-set (to-set attributes)]
    (make-ordered-context objects attributes
                          (set-of [g m] | g object-set, m attribute-set, :when (> fill-rate (rand))))))
