(in-package #:prod-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <var-set> (<prod-set>)
  ((element-type :initarg :element-type  :initform 'vector :reader element-type :type symbol))
   
  (:documentation
   "A subclass of <prod-set> that uses sequences to represent instantiations.

Create with make-instance or using make-var-set.  Initargs
:sets -            sequence of sets as in <prod-set>
:element-type -    should be 'list or 'vector (default is 'vector).
:iterate-quickly - as in <prod-set>.  optional and defaults to nil.
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after
    ((s <var-set>) &rest args &key (element-type 'vector))
  (declare (ignore args))
  (let* ((d (dim s)))
    (set-inst-acc 
     (ecase element-type
       (vector (make-vec-accessors d))
       (list (make-list-accessors d)))
     s)
    s))










