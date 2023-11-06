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
          ^String (context-to-string (make-context-nc (objects ctx)
                                                      (attributes ctx)
                                                      (incidence ctx))
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

(defmethod make-ordered-context [Object Object clojure-coll clojure-seq clojure-seq]
  [objects attributes incidence order-on-objects order-on-attributes]
  (let [object-set (to-set objects)
        attribute-set (to-set attributes)
        incidence-set (set-of [g m] [[g m] incidence
                                     :when (and (contains? object-set g)
                                                (contains? attribute-set m))])]
    (Ordered-Context. object-set attribute-set incidence-set order-on-objects order-on-attributes)))

(defmethod make-ordered-context [Object Object clojure-fn clojure-seq clojure-seq]
  [objects attributes incidence order-on-objects order-on-attributes]
  (Ordered-Context. (set objects)
                    (set attributes)
                    (fn [[g m]]
                      (incidence g m))
                    objects
                    attributes))

(comment (defmethod make-ordered-context [Object Object clojure-coll clojure-fn clojure-fn]
           [objects attributes incidence order-on-objects order-on-attributes]
           (let [object-set (to-set objects)
                 attribute-set (to-set attributes)
                 incidence-set (set-of [g m] [[g m] incidence
                                              :when (and (contains? object-set g)
                                                         (contains? attribute-set m))])])))

(defmethod make-ordered-context :default [objects attributes incidence]
  (illegal-argument "The arguments " objects ", " attributes " and " incidence 
                    " are not valid for an ordered context."))

(defmethod make-ordered-context :default [objects attributes incidence order-on-objects order-on-attributes]
  (illegal-argument "The arguments " objects ", " attributes ", " incidence ", " order-on-objects " and " order-on-attributes
                    " are not valid for an ordered context."))

(defn make-ordered-context-from-matrix
  "Given objects G, attributes M and an incidence matrix, constructs the 
  corresponding context. G and M may also be numbers where they represent (range G)
  and (range M) respectively."
  [G M bits]
  (assert (forall [x bits] (or (= 1 x) (= 0 x)))
          "All entries given must be either 0 or 1.")
  (let [G (ensure-seq G),
        M (ensure-seq M),
        m (count G),
        n (count M)]
    (assert (= (* m n) (count bits))
            "Number of objects and attributes does not match the number of entries.")
    (make-ordered-context-nc G M
                             (set-of [a b] [i (range (count G)),
                                            j (range (count M)),
                                            :when (= 1 (nth bits (+ (* n i) j)))
                                            :let [a (nth G i),
                                                  b (nth M j)]]))))
