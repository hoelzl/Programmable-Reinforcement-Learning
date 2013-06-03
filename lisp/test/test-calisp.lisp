(in-package :common-lisp-user)

(defpackage #:test-calisp
  (:use #:common-lisp
	#:rbe
	#:calisp-user
	#:prob
	#:thread-dec-q
	#:rbe-dec
	#:utils
	#:rbe-prog
	#:rbe-linear-features
	#:inst-vars))


(in-package #:test-calisp)
;;; TODO: Change SETF into DEFPARAMETER and rename variables. --tc

(defparameter *num-peas* 2)
(defparameter *wm* (make-array '(1 4) :initial-element 't))
(defparameter *base-loc* '(0 1))
(defparameter *forest-locs* '((0 0)))
(defparameter *mine-locs* '((0 3)))
(defparameter *max-gold* 2)
(defparameter *max-wood* 2)


(defparameter *e*
  (make-rbe-env *wm* *num-peas* *max-gold* *max-wood* *base-loc* *forest-locs* *mine-locs*))
(defparameter *p* (make-rbe-prog))
(defparameter *q* (make-rbe-crl-q-function *max-gold* *max-wood*))
(defparameter *q-dec* (make-rbe-dec-q-function *max-gold* *max-wood* *num-peas*))
(setf (crlq:debug-mode *q*) t)


(force-format t "~&This should print about 100 '.'s~%")
;(calisp:io-interface *p* *e* nil)

(defparameter *tq-learner* (make-instance '<threadwise-decomposed-q-learner>
		   :q-function *q-dec*
		   :reward-decomposer #'rbe-reward-decomp))
(defparameter *sq* (make-instance 'csq:<smdpq> :q-fn *q*))
(calisp-user:learn *p* *e* 'random (list *sq* *tq-learner*) 5000
                   :hist-length 50 :step-print-inc 100 :episode-print-inc 10)
(defparameter *sq-q-hist* (get-q-hist *sq*))
(defparameter *sq-pol-hist* (get-policy-hist *sq*))
(defparameter *tq-q-hist* (get-q-hist *tq-learner*))
(defparameter *tq-pol-hist* (get-policy-hist *tq-learner*))
(defparameter *sq-rews* (calisp-user:evaluate *p* *e* *sq-pol-hist* :num-steps 50 :num-trials 1))
(defparameter *tq-rews* (calisp-user:evaluate *p* *e* *tq-pol-hist* :num-steps 50 :num-trials 1))
(pprint *sq-rews*)
(pprint *tq-rews*)
