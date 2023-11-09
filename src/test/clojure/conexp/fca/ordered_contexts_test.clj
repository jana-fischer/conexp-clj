;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.ordered-contexts-test
  (:use clojure.test)
  (:require [conexp.fca.contexts :refer :all]
            [conexp.fca.ordered-contexts :refer :all])
  (:import [conexp.fca.contexts Formal-Context]
           [conexp.fca.ordered_contexts Ordered-Context]))

(deftest test-Ordered-Context-equals
  (is (= (Ordered-Context. #{} #{} #{} [] [])
         (Ordered-Context. #{} #{} #{} [] [])))
  (is (= (Ordered-Context. #{0 1} #{'a 'b} #{[0 'a] [1 'b]} [0 1] '[a b])
         (Ordered-Context. #{0 1} #{'a 'b} #{[0 'a] [1 'b]} [0 1] '[a b])))
  (is (= (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])
         (Ordered-Context. #{0 1 2} #{0 1 2} (fn [[g m]] (= g m)) [0 1 2] [0 1 2])))
  (is (= (Ordered-Context. #{0 1} #{0 1} #{[0 0] [1 1]} [0 1] [1 0])
         (Ordered-Context. #{0 1} #{0 1} #{[0 0] [1 1]} <= >=)))
  (is (= (Ordered-Context. [0 1] '[a b] #{[0 'a] [1 'b]} [0 1] '[a b])
         (Ordered-Context. [1 0] '[b a] #{[0 'a] [1 'b]} [0 1] '[a b])))
  (is (not= (Ordered-Context. [0 1] '[a b] #{[0 'a] [1 'b]} [0 1] '[a b])
            (Ordered-Context. [1 0] '[a b] #{[0 'a] [1 'b]} [1 0] '[a b])))
  (is (not= (Ordered-Context. [0 1] '[a b] #{[0 'a] [1 'b]} [0 1] '[a b])
            (Ordered-Context. [0 1] '[b a] #{[0 'a] [1 'b]} [0 1] '[b a])))
  (is (not= (Ordered-Context. [0 1] '[a b] #{[0 'a] [1 'b]} [0 1] '[a b])
            (Ordered-Context. [0 1] '[a b] #{[0 'a] [1 'a]} [0 1] '[a b])))
  (is (not= (Ordered-Context. [0 1] '[a b] #{[0 'a] [1 'b]} [0 1] '[a b])
            (Formal-Context. [0 1] '[a b] #{[0 'a] [1 'a]})))
  (is (not= (Ordered-Context. [] [] [] [] [])
            (Object.))))

(deftest test-Ordered-Context-hashCode
  (let [context1 (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])
        context2 (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])
        context3 (Ordered-Context. #{0 1 2} #{0 1 2} (fn [[g m]] (= g m)) [0 1 2] [0 1 2])]
    (is (= (hash context1) (hash context2)))
    (is (not (= (hash context1) (hash context3))))))

(deftest test-order-on-objects
  (let [context1 (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])
        context2 (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} < <)]
    (is (= (order-on-objects context1)
           [0 1 2]))
    (is (= (order-on-objects context2)
           <))))

(deftest test-order-on-attributes
  (let [context1 (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])
        context2 (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} < <)]
    (is (= (order-on-attributes context1)
           [0 1 2]))
    (is (= (order-on-attributes context2)
           <))))

(deftest test-ordered-context?
  (is (ordered-context? (Ordered-Context. [] [] [] [] [])))
  (is (ordered-context? (Ordered-Context. [0 1] '[a b] #{[0 'a] [1 'b]} [0 1] '[a b])))
  (is (not (ordered-context? (Object.))))
  (is (not (ordered-context? (Formal-Context. [0 1] '[a b] #{[0 'a] [1 'b]})))))

;; Constructor tests

(deftest test-make-ordered-context-nc
  (is (= (make-ordered-context-nc [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]})
         (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])))
  (is (= (make-ordered-context-nc [0 1 2] [0 1 2] (fn [[g m]] (= g m)))
         (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])))
  (is (= (make-ordered-context-nc #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])
         (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])))
  (is (= (make-ordered-context-nc #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} >= <=)
         (Ordered-Context. #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} >= <=))))

(deftest test-make-ordered-context
  (is (= (make-ordered-context [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]})
         (Ordered-Context. [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])))
  (is (= (make-ordered-context [0 1 2] [0 1 2] =)
         (Ordered-Context. [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])))
  (is (= (make-ordered-context [0 1 2] [0 1 2] #{})
         (Ordered-Context. [0 1 2] [0 1 2] #{} [0 1 2] [0 1 2])))
  (is (= (make-ordered-context [0 1 2] [0 1 2] #{[0 0] [0 1] [0 2]
                                                 [1 0] [1 1] [1 2]
                                                 [2 0] [2 1] [2 2]})
         (Ordered-Context. [0 1 2] [0 1 2] #{[0 0] [0 1] [0 2]
                                             [1 0] [1 1] [1 2]
                                             [2 0] [2 1] [2 2]}
                           [0 1 2] [0 1 2])))
  (is (= (make-ordered-context #{0 1 2} #{0 1 2} #{[0 0] [1 1] [2 2]} > >)
         (Ordered-Context. [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]} [2 1 0] [2 1 0])))
  (is (= (make-ordered-context #{0 1 2} #{0 1 2} = > >)
         (Ordered-Context. [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]} [2 1 0] [2 1 0])))
  (is (thrown? IllegalArgumentException 
               ;; objects and attributes cannot be a map
               (make-ordered-context {0 1} {0 1} #{[0 0] [1 1]})))
  (is (thrown? IllegalArgumentException 
               ;; objects and attributes may not be without order
               (make-ordered-context #{0 1} #{0 1} #{[0 0] [1 1]})))
  (is (thrown? IllegalArgumentException 
               ;; object- and attribute-order may not be a set
               (make-ordered-context {0 1} {0 1} #{[0 0] [1 1]} #{0 1} #{0 1})))
  (is (thrown? IllegalArgumentException
               ;; incidence of a context must be a function or set
               (make-ordered-context [0] [1] 2)))
  (is (thrown? AssertionError
               ;; All objects need to be considered in the order-on-objects.
               (make-ordered-context [0 1 2] [0 1 2] = [0 1] [0 1])))
  (is (thrown? AssertionError
               ;; All attributes need to be considered in the order-on-attributes.
               (make-ordered-context [0 1 2] [0 1 2] = [0 1 2] [0 1]))))

;;

(deftest test-make-ordered-context-from-matrix
  (is (= (make-ordered-context-from-matrix [0 1 2] [0 1 2] [1 0 0 0 1 0 0 0 1])
         (Ordered-Context. [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])))
  (is (= (make-ordered-context-from-matrix #{0 1 2} #{0 1 2} [1 0 0 0 1 0 0 0 1] [0 1 2] [0 1 2])
         (Ordered-Context. [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])))
  (is (= (make-ordered-context-from-matrix #{0 1 2} #{0 1 2} [1 0 0 0 1 0 0 0 1] < <)
         (Ordered-Context. [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]} [0 1 2] [0 1 2])))
  (is (thrown? AssertionError
               ;; matrix should only contain 0 and 1
               (make-ordered-context-from-matrix [0 1 2] [0 1 2] [1 0 0 0 1 0 0 0 2])))
  (is (thrown? AssertionError
               ;; incorrect matrix size
               (make-ordered-context-from-matrix [0 1 2] [0 1 2] [1 0 0 0 1 0 0 0]))))

(deftest test-rand-ordered-context
  (let [context (rand-ordered-context [0 1 2 3] [4 5 6 7] 0.5)]
    (is (ordered-context? context))
    (is (= [0 1 2 3] (order-on-objects context)))
    (is (= [4 5 6 7] (order-on-attributes context)))))

;;

(deftest test-rename-objects
  (let [context1 (make-ordered-context [0 1] [0 1 2 3 4]
                                       #{[0 0] [0 1] [1 2] [0 3] [1 3]})
        context2 (make-ordered-context [1 2] [0 1 2 3 4]
                                       #{[1 0] [1 1] [2 2] [1 3] [2 3]})
        context3 (make-ordered-context [0 1] [0 1 2 3 4]
                                       #{[0 0] [0 1] [1 2] [0 3] [1 3]} < <)]
    (is (= (rename-ordered-objects context1 #(get {0 1 1 2} %))
           context2))
    (is (= (rename-ordered-objects context3 #(get {0 1 1 2} %))
           context2))))

;;; Test that functions from conexp.fca.contexts work with Ordered-Contexts:

(deftest test-incident?
  (let [context (make-ordered-context [0 1 2] [0 1 2] #{[0 0] [1 1] [2 2]})]
    (is (incident? context 0 0))
    (is (not (incident? context 0 1)))))

(deftest test-incidence-relation
  (let [context1 (make-ordered-context [0 1 2] [0 1 2] =)
        context2 (make-ordered-context [0 1 2] '[a b c] #{})]
    (is (= (incidence-relation context1)
           #{[0 0] [1 1] [2 2]}))
    (is (= (incidence-relation context2)
           #{}))))

(deftest test-context-size
  (let [context (make-ordered-context [0 1] '[a b c d e]
                                      #{[0 'a] [0 'b] [1 'c] [0 'd] [1 'd]})]
    (is (= (context-size context)
           '(2 5 0.5)))))

(deftest test-subcontext?
  (let [context1 (make-ordered-context [0 1 2] [0 1 2] =)
        context2 (make-ordered-context [0 1] [0 1] =)
        context3 (make-ordered-context [0 1] [0 1] <=)]
    (is (subcontext? context2 context1))
    (is (not (subcontext? context3 context1)))))

(deftest test-object-derivation
  (let [context (make-ordered-context [0 1] '[a b c d e]
                                      #{[0 'a] [0 'b] [1 'c] [0 'd] [1 'd]})]
    (is (= (object-derivation context #{})
           #{'a 'b 'c 'd 'e}))
    (is (= (object-derivation context #{0})
           #{'a 'b 'd}))
    (is (= (object-derivation context #{0 1})
           #{'d}))))

(deftest test-restrict-concept
  (let [context (make-ordered-context [0 1] [0 1] <=)
        concept [#{0 1 2} #{}]]
    (is (= (restrict-concept concept context)
           [#{0 1} #{}]))))

(deftest test-attribute-derivation
  (let [context (make-ordered-context [0 1] '[a b c d e]
                                      #{[0 'a] [0 'b] [1 'c] [0 'd] [1 'd]})]     
    (is (= (attribute-derivation context #{})
           #{0 1}))
    (is (= (attribute-derivation context #{'a})
           #{0}))
    (is (= (attribute-derivation context #{'a 'c})
           #{}))))

(deftest test-concept?
  (let [context (make-ordered-context [0 1 2] [0 1 2] <=)]
    (is (concept? context [#{0} #{0 1 2}]))
    (is (not (concept? context [#{1} #{1}])))))

(deftest test-object-concept
  (let [context (make-ordered-context [0 1] '[a b c d e]
                                      #{[0 'a] [0 'b] [1 'c] [0 'd] [1 'd]})]
    (is (= (object-concept context 0)
           [#{0} #{'a 'b 'd}]))
    (is (= (object-concept context 1)
           [#{1} #{'c 'd}]))))
  
(deftest test-attribute-concept
  (let [context (make-ordered-context [0 1] '[a b c d e]
                                      #{[0 'a] [0 'b] [1 'c] [0 'd] [1 'd]})]     
    (is (= (attribute-concept context 'a)
           [#{0} #{'a 'b 'd}]))
    (is (= (attribute-concept context 'c)
           [#{1} #{'c 'd}]))))

(deftest test-object-clarified?
  (let [context1 (make-ordered-context [0 1 2] '[a b c] 
                                       #{[0 'a] [1 'b] [1 'c] [2 'a] [2 'c]})
        context2 (make-ordered-context [0 1 2] '[a b c] 
                                       #{[0 'a] [1 'b] [1 'c] [2 'a]})]
    (is (object-clarified? context1))
    (is (not (object-clarified? context2)))))

(deftest test-attribute-clarified?
  (let [context1 (make-ordered-context [0 1 2] '[a b c] 
                                       #{[0 'a] [1 'b] [1 'c] [2 'a] [2 'c]})
        context2 (make-ordered-context [0 1 2] '[a b c] 
                                       #{[0 'a] [1 'b] [1 'c] [2 'a]})]
    (is (attribute-clarified? context1))
    (is (not (attribute-clarified? context2)))))

(deftest test-context-clarified?
  (let [context1 (make-ordered-context [0 1 2] '[a b c] 
                                       #{[0 'a] [1 'b] [1 'c] [2 'a] [2 'c]})
        context2 (make-ordered-context [0 1 2] '[a b c] 
                                       #{[0 'a] [1 'b] [1 'c] [2 'a]})]
    (is (context-clarified? context1))
    (is (not (context-clarified? context2)))))

