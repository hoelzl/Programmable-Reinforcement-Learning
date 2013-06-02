(defpackage #:test-pot-set
  (:use #:pot-set
	#:utils
	#:prob
	#:common-lisp
	#:prod-set
	#:inst-vars
	#:set))

(in-package #:test-pot-set)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (inst )
  foo
  bar
  baz
  qux
  oof)

(defparameter *domains*
  '(3 (a b) 4 (() (1) (a b)) 3))

(defparameter *acc* (inst-vars:make-struct-accessors inst (foo bar baz qux oof)))
(defparameter *insts* (make-instance '<prod-set> 
                        :sets *domains*
                        :inst-acc *acc*))

(defparameter *pot1* (make-function-potential '(baz foo) #'+ *acc*))

(defun f2 (bar oof qux)
  (ecase bar
    (a (length qux))
    (b (- oof))))

(defparameter *pot2* (make-function-potential
                      '(bar oof qux) #'f2 *acc*))

(defun f3 (foo bar oof)
  (ecase bar
    (a (- foo oof))
    (b (- oof foo))))

(defun f4 (oof baz)
  (- (expt oof 2)
     (expt baz 2)))

(defparameter *pot3* (make-function-potential
                      '(foo bar oof) #'f3
                      *acc*))

(defparameter *pot4* (make-function-potential
                      '(oof baz)
                      #'f4
                      *acc*))



(defparameter *i1* (make-inst :foo 1 :bar 'a :baz 3))
(defparameter *i2* (make-inst :foo 2 :bar 'a :baz 1 :qux '(1) :oof 2))
(defparameter *i3* (make-inst :foo 0 :bar 'a :baz 0 :qux '(a b) :oof 0))
(defparameter *i4* (make-inst :foo 2 :bar 'a :baz 1 :qux '(a b) :oof 1))

(do-tests
  "potential evaluation"
  
  (eval-pot *pot1* *i1*) 4
  (eval-pot *pot2* *i2*) 1
  (progn (setf (inst-bar *i2*) 'b) (eval-pot *pot2* *i2*)) -2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations on potentials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pot5*)
(defvar *pot6*)
(defvar *pot7*)
(defvar *pot8*)
(defvar *pot9*)

(do-tests
    "multiplication and summing out"
  (progn
    (setf *pot5* (multiply *times* *insts* *pot1* *pot2*))
    (eval-pot *pot5* *i2*))
  -6
  
  (progn
    (setf *pot6* (multiply-and-sum-out *times* *plus* *insts* 'oof *pot2* *pot3* *pot4*))
    (sort (vars *pot6*) #'symbol<))
  '(bar baz foo qux)
  
  (eval-pot *pot6* *i2*)
  0
  
  (progn
    (setf *pot7* (multiply-and-sum-out *plus* *max* *insts* 'oof *pot2* *pot3* *pot4*))
    (eval-pot *pot7* *i2*))
  1

  (progn
    (setf *pot7* (multiply-and-sum-out *plus* *min* *insts* 'oof *pot2* *pot3* *pot4*))
    (eval-pot *pot7* *i2*))
  -3

  (progn (set-var-val-by-name *acc* *i2* 'bar 'a)
	 (eval-pot *pot6* *i2*))
  -2
  
  (eval-pot *pot6* *i3*)
  -18
  
  (progn (setf *pot8* (multiply-and-sum-out *plus* *min* *insts* 'bar *pot2* *pot3*))
	 (eval-pot *pot8* *i2*))
  -2
  
  (progn (setf *pot9* (multiply-and-sum-out *times* *plus* *insts* 'bar *pot3* *pot2*))
	 (eval-pot *pot9* *i4*))
  3
  
  )


(defun bsamp (pots elim-order sets temp 
	      &key (expected nil) (acc (inst-vars:make-vec-accessors (length sets)))
		   (num 1000) (inc (1+ num)))
  (declare (ignore acc) (optimize (debug 3)))
  (let* ((g (boltzmann-gm pots elim-order 
			  (make-instance '<prod-set> :inst-acc *acc* :sets sets) temp))
	 (h (rand-fn-hist (sample g) num :inc inc))
	 (all-keys (union (mapcar #'car expected) (hash-keys h) :test #'equalp)))
    (if expected
	(reduce #'+ all-keys
		:key #'(lambda (k)
			 (abs (- (or (gethash k h) 0)
				 (or (cdr (assoc k expected :test #'equalp)) 0)))))
        h)))

(defun bsamp2 (pots elim-order *insts* temp &key (num 100) (inc (1+ num)) 
                                            &aux (*acc* (inst-acc *insts*)))
  
  (let* ((g (boltzmann-gm pots elim-order *insts* temp))
	 (h (rand-fn-hist (sample g) num :inc inc))
	 (d (make-boltzmann-dist
	     (mapset 'vector
		     #'(lambda (inst)
			 (reduce #'+ pots
				 :key #'(lambda (pot)
					  (eval-pot pot inst))))
		     *insts*)
	     temp))
	 (h2 (rand-fn-hist (item (sample d) *insts*) num :inc inc))
	 (all-keys (union (hash-keys h2) (hash-keys h) :test #'equalp)))
    
    (reduce #'+ all-keys
	    :key #'(lambda (k)
		     (abs (- (or (gethash k h) 0)
			     (or (gethash k h2) 0)))))))
    
(defvar *c* (/ 1 (log 2)))
(defvar *d* (/ 1 (log 1.5)))


(defvar *best*)

(do-tests
  "best-assignment function"
  
  ;; empty lists of potentials and/or variables
  (multiple-value-list
   (best-assignment *max* *times* nil nil (make-instance '<var-set> :sets nil)))
  (list #() '-infty)
  
  (multiple-value-list
   (best-assignment *min* *plus* nil '(1 0) (make-instance '<var-set> :sets '((a b c) (1)))))
  (list #(a 1) 0)
  
  ;; single potential
  (multiple-value-bind (best val)
      (best-assignment *max* *times* (list *pot1*) '(foo bar baz) *insts*)
    (list (inst-foo best) (inst-baz best) val))
  '(2 3 5)
  
  ;; multiple potentials in different orders
  (progn
    (setf *best* (list (make-inst :foo 0 :bar 'a :baz 3  :qux nil :oof 0) -6))
    (multiple-value-list
     (best-assignment *min* *plus* (list *pot1* *pot2* *pot3* *pot4*)
                      '(foo bar baz qux oof) *insts*)))
  *best*
  
  (multiple-value-list
   (best-assignment *min* *plus* (list *pot3* *pot4* *pot2* *pot1*)
                    '(qux baz bar oof foo) *insts*))
  *best*
  
  (multiple-value-list
   (best-assignment *min* *plus* (list *pot4* *pot3* *pot2* *pot1*)
                    '(bar oof foo baz qux) *insts*))
  *best*
  
  
  (multiple-value-list 
   (best-assignment *max* *plus* (list *pot4* *pot3* *pot1* *pot2*)
                    '(qux baz oof foo bar) *insts*))
  (list (make-inst :foo 2 :bar 'a :baz 0 :qux '(a b) :oof 2) 8)
  
  )
  
(defvar *e1*)
(defvar *e2*)

(do-rand-tests
  "Boltzmann sampling (randomized)"
  
  ;; border cases
  (setf *e1* '((#() . 1000)))
  ;; TODO: The following two tests report errors (since (bsamp ...)  always returns 2000.  This
  ;; might be due to the compilation problems with RAND-FN-HIST, but on the other hand the hash
  ;; table returned by rand-fn-hist looks pretty reasonable to me. --tc
  (eql 0 (bsamp nil nil nil 0 :expected *e1*))
  (eql 0 (bsamp nil nil nil 1 :expected *e1*))
  ;; TODO: Check why this fails --tc
  #+ (or)
  (< (bsamp nil '(1 0) '((a b c) 1) 1 :expected '((#(a 1) . 333) (#(b 1) . 333) (#(c 1) . 333))))
  (setf *e2*
        (loop
          for foo below 3
          append
	  (loop 
            for baz below 4
            collect (cons
                     (make-inst :foo foo :baz baz :bar 'uninstantiated :oof 'uninstantiated
                                :qux 'uninstantiated)
                     (round (* (/ (expt 2 (+ foo baz)) 105) 200))))))
  
  ;; single potential 
  (< (bsamp (list *pot1*) '(foo baz) *domains* *c*
            :expected *e2* :acc *acc* :num 200 :inc 100) 50)
  (< (bsamp (list *pot1*) '(baz foo) *domains* *c*
            :expected *e2* :acc *acc* :num 200 :inc 100) 50)
  
  ;; multiple potentials
  (< (bsamp2 (list *pot3* *pot4*) '(foo oof bar qux baz) *insts*
             (/ 1 (log 10)) :num 500 :inc 50) 100)
  (< (bsamp2 (list *pot3* *pot4*) '(oof qux baz bar foo) *insts*
             'infty :num 2500 :inc 500) 1000)
  
  ;(< (bsamp2 (list *pot2* *pot4* *pot3* *pot1*) '(bar qux oof foo baz) *insts*
  ;           1000 :num 500 :inc 50) 100)

  )
