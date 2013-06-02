(defpackage #:test-alisp
  (:use 
   #:utils
   #:common-lisp
   #:td-taxi-env
   #:td-taxi-prog
   #:alisp-user
   #:td-taxi-prog-features
   #:policy
   ))

(in-package #:test-alisp)

(format t "~&Testing ALisp code")
(fill-format-nl #\= 60)
(format t "~&Creating objects")
(defparameter *e* (td-taxi-env:make-example-env1))
(defparameter *p* #'td-taxi-prog)

(defparameter *smdpq* (alisp-smdpq:make-smdpq-alg :hist-out-dir "test/temp"))
(defparameter *hordq* (make-instance 'ahq:<hordq>))
(defparameter *gs* (alisp-gold-standard:make-alisp-gold-standard-learning-alg))
(defparameter *hsa* (make-instance 'ahq:<hordq> :features *featurizer*))

(terpri)
(alisp-user:learn *p* *e* 'random (list *hordq* *smdpq* *gs* *hsa*) 20000 :hist-length 20)

(terpri)
(defparameter *sq-pol-hist* (alisp-user:get-policy-hist *smdpq*))
(defparameter *sq-q-hist* (get-q-hist *smdpq*))
(defparameter *sq-rews*
  (alisp-user:evaluate *p* *e* *sq-pol-hist*
                       :num-steps 25 :num-trials 5))
(defparameter *hq-rews*
  (alisp-user:evaluate *p* *e* (alisp-user:get-policy-hist *hordq*)
                       :num-steps 25 :num-trials 5))
(defparameter *gs-rews*
  (alisp-user:evaluate *p* *e* (alisp-user:get-policy-hist *gs*)
                       :num-steps 25 :num-trials 5))
(defparameter *hqs-rews* 
  (alisp-user:evaluate *p* *e* (alisp-user:get-policy-hist *hsa*)
                       :num-steps 25 :num-trials 5))

(format t "~%~%Learning curves for SMDPQ, HORDQ, HORDQ-SA, and GS are ~a"
	(map 'vector #'list *sq-rews* *hq-rews* *hqs-rews* *gs-rews*))

;; since smdpq uses temporary files, delete them
(reset *smdpq*)
