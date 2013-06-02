(defpackage #:test-exp-pol
  (:use #:common-lisp
	#:utils
	#:rl-user
	#:q-learning
	#:td-taxi-env
	#:gold-standard))

(in-package #:test-exp-pol)

(format t "~&Testing RL Code")
(fill-format-nl #\= 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create environment, compute ground truth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *env* (td-taxi-env:make-example-env1))
;(setf v-true (make-array 3 :initial-contents '(22.3 19.8 0)))
;;(setf q-true (dp:q-from-v mdp-test-envs:*test-mdp-1* v-true))
;(setf e (make-instance 'mdp-env:<mdp-env> :mdp mdp-test-envs:*test-mdp-1*
;			       :init-dist #(.5 .5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; learning exp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun evaluate-alg (alg)
  (declare (ignore alg)))

(defparameter *num-steps-learning* 10000)
(defparameter *hist-length* 20)

(defparameter *featurizer* (td-taxi-flat-lfa:make-taxi-featurizer *env*))
(defparameter *lq*
  (make-instance 'q-fn:<env-q-function>
    :env *env* :env-name 'env :featurizer *featurizer*
    :featurizer-name 'featurizer
    :fn-approx (make-instance 'fn-approx:<linear-fn-approx> :dim 7)))

(defparameter *q-learner*
  (make-instance '<q-learning> :env *env* :lrate .01 :discount 1.0))

(defparameter *lfa-q-learner*
  (make-instance '<q-learning> :env *env* :lrate .01 :discount 1.0 :q-fn *lq*))

(defparameter *api-learner*
  (make-instance 'api:<approx-pol-it> :env *env* :pol-imp-int 100 :pol-switch 500))

(defparameter *f* t)

;(with-outfile (*f* "scratch/test-gs.out")

(defparameter *gs-learner*
  (gold-standard:make-gold-standard-learning-algorithm :debug-str *f*))

(defparameter *env-obs*
  (env-observer:make-env-observer *f*))

(format t "~&Learning with random exploration")

(learn *env* 'random *lfa-q-learner* *num-steps-learning*
       :hist-length *hist-length* :ep-print-inc 10 :step-print-inc 100)

; (on-policy-learning e *api-learner* *num-steps-learning*
; 		    :hist-length *hist-length* :ep-print-inc 100 :step-print-inc 100)

(defparameter *qh* (get-q-hist *lfa-q-learner*))

(defparameter *ph* (get-policy-hist *lfa-q-learner*))

(defparameter *exp-pol*
  (make-instance 'exp-pol:<epsilon-boltzmann-exp-pol>
    :q-learning-alg *lfa-q-learner*
    :temp-fn (exp-pol:make-temp-decay-fn 1.001 2000 2 .1)
    :epsilon-decay-fn (exp-pol:make-linear-epsilon-decay-fn 1000 .2)))

(format t "~2&Learning with Boltzmann exploration")
(learn *env* *exp-pol* *lfa-q-learner* *num-steps-learning* 
       :hist-length *hist-length* :ep-print-inc 10 :step-print-inc 100)

(defparameter *qh-exp* (get-q-hist *lfa-q-learner*))

(defparameter *ph-exp* (get-policy-hist *lfa-q-learner*))

;(defparameter *lqh* (get-q-hist lfa-lfa-q-learner))

;(defparameter *gh* (get-q-hist *gs-learner*))

;(defparameter *ah* (get-q-hist *api-learner*))

(defvar *q-rews*)
(defvar *q-exp-rews*)

(format t "~&Q-learning algorithm learning curve is~&~W"
	(setf *q-rews*
              (evaluate *env* *ph* :num-steps 25 
                                   :num-trials 10 :env-display-stream nil 
                                   :pause-after-actions nil)))

(format t "~&Exploring Q-learning algorithm learning curve is~&~W"
	(setf *q-exp-rews*
              (evaluate *env* *ph-exp* :num-steps 25 
                                       :num-trials 10 :env-display-stream nil 
                                       :pause-after-actions nil)))










