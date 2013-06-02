;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env/fully-observable-env.lisp
;; defines the <fully-observable-env> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:env)

(defclass <fully-observable-env> (<env>)
  ()
  (:documentation "Class <fully-observable-env> (<env>)
A fully observable environment is one where the percept equals the current state."))

(defmethod sample-percept ((e <fully-observable-env>) s a r s2)
  (declare (ignore s a r))
  s2)

(defmethod sample-init-percept ((e <fully-observable-env>) s)
  s)

;;; TODO: This should probably be in env.lisp, since it is called by CURRENT-EFFECTORS.
(defgeneric get-state (e)
  (:documentation "get-state FULLY-OBSERVABLE-ENV
Return the current state of FULLY-OBSERVABLE-ENV.  The returned object should be treated as
immutable, since the environment might hold a reference to it.")
  (:method ((e <fully-observable-env>))
	   (state e)))


(defmethod set-state (s e)
  "set-state STATE FULLY-OBSERVABLE-ENVIRONMENT
Sets state and also takes care of setting the percept."
  (call-next-method)
  (set-last-percept s e))
