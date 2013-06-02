(defpackage #:test-tree
  (:use #:common-lisp
	#:utils
	#:tree))

(in-package #:test-tree)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic tree construction, consistency
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *n1* (make-node :node-label 'foo))
(defparameter *n2* (add-new-child *n1* nil 'bar))
(defparameter *n3* (add-new-child *n1* nil 'baz))
(defparameter *n4* (add-new-child *n2* 'qux-edge 'qux))
(defparameter *n5* (add-new-child *n2* 'oof-edge 'oof))

(defparameter *e1* (make-edge *n5* *n1*))
(defparameter *e2* (find 'qux-edge (child-edges *n2*) :key #'edge-label))
(defparameter *e3* (parent-edge *n5*))

(do-tests
  "consistency checking"
  (is-inconsistent-tree *n1* t) nil
  (is-inconsistent-tree *n1* nil) nil
  (is-inconsistent-tree *n2* t) 'not-root
  (is-inconsistent-tree *n2* nil) nil
  
  (progn
    (push *e1* (child-edges *n5*))
    (is-inconsistent-tree *n1* t))
  'cycle
  
  (progn 
    (deletef (child-edges *n5*) *e1*)
    (is-inconsistent-tree *n1* t))
  nil
  
  (progn
    (setf (tail *e2*) *n1*)
    (is-inconsistent-tree *n1* t))
  'incorrect-child-edge
  
  (progn
    (setf (tail *e2*) *n2*)
    (is-inconsistent-tree *n2* nil))
  nil
  
  (progn 
    (setf (parent-edge *n5*) (parent-edge *n3*))
    (is-inconsistent-tree *n1* t))
  'incorrect-parent-edge)

(do-tests
  "tree traversal"
  (progn
    (setf (parent-edge *n5*) *e3*)
    (map-preorder 'list #'node-label *n1*))
  '(foo bar qux oof baz)
  
  (map-postorder 'list #'node-label *n1*)
  '(qux oof bar baz foo))

(defvar *tr*)
(defvar *deleted*)

(do-tests
  "tree modification"
  (progn
    (add-subtree *n3* nil (copy-subtree *n2*) 0)
    (setf (node-label (get-child (get-child *n3* 0) 1)) 'ooof)
    (map-preorder 'list #'node-label *n1*))
  '(foo bar qux oof baz bar qux ooof)
  
  (progn
    (setf *tr* (copy-subtree (get-child *n3* 0)))
    (add-subtree *n1* nil *tr* 1)
    (map-preorder 'list #'node-label *n1*))
  '(foo bar qux oof bar qux ooof baz bar qux ooof)
  
  (progn
    (setf *deleted* (remove-subtree (get-child *n1* 0)))
    (map-preorder 'list #'node-label *n1*))
  '(foo bar qux ooof baz bar qux ooof)
  
  (map-postorder 'list #'node-label *n1*)
  '(qux ooof bar qux ooof bar baz foo)
  
  (is-consistent-tree *deleted* t)
  t
  
  (map-preorder 'list #'node-label *deleted*)
  '(bar qux oof)
  
  )
  
  
  
  
  

    
