(in-package #:common-lisp-user)

(defpackage #:mdp-env
  (:documentation "mdp-env package.  

Exported ypes
<mdp-env> - a subclass of <fully-observable-env> whose behaviour is governed by an MDP

In addition to general <env> operations, also provides
mdp -               get underlying mdp of <mdp-env>
make-2tbn-mdp-env - create a <mdp-env> based on a 2TBN

")
  (:use #:common-lisp
        #:utils
	#:create-env)
  (:export 
   #:mdp
   #:<mdp-env>
   #:<smdp-env>
   #:make-2tbn-mdp-env))


(in-package #:mdp-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <mdp-env> (<fully-observable-env>)
  ((m :initarg :mdp
      :initform (required-initarg :mdp)
      :reader mdp)
   (init-prob-dist :initarg :init-dist
                   :initform (required-initarg :mdp)
		   :reader init-dist
		   :type prob:<prob-dist>))
  (:documentation "Class <mdp-env> (<fully-observable-env>)
Initargs
:m - MDP that this environment is based on.
:init-dist - Distribution over initial state"))


;; reset the state to begin with
(defmethod initialize-instance :after ((e <mdp-env>) &rest args)
  (declare (ignore args))
  (reset e))

   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations on the underlying MDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; TODO: Having different generics with the same name in different packages seems really ugly.
;;; Either rename them into env-trans-prob or unify with mdp:trans-prob. --tc
(defgeneric trans-prob (e s a d))

(defmethod trans-prob ((e <mdp-env>) s a d)
  (mdp:trans-prob (mdp e) s a d))

(defmethod at-terminal-state ((e <mdp-env>))
  (mdp:terminal? (mdp e) (get-state e)))

(defmethod sample-init ((e <mdp-env>))
  (prob:sample (init-dist e)))

(defgeneric reset-to-state (e s))

(defmethod reset-to-state ((e <mdp-env>) s)
  (set-state s e))

(defmethod sample-next ((e <mdp-env>) s a)
  (let* ((m (mdp e))
	 (d (prob:sample (mdp:trans-dist m s a))))
    (values d (mdp:reward m s a d))))

(defmethod avail-actions ((e <mdp-env>) s)
  (mdp:avail-actions (mdp e) s))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smdp-env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <smdp-env> (<mdp-env>)
  ()
  (:documentation "Class <smdp-env> (<mdp-env>)
Like an <mdp-env> except works on any SMDP.  Sample-next returns the duration as a third
argument.  A bit hacky and might change in future."))

(defmethod sample-next ((e <smdp-env>) s a)
  (let* ((m (mdp e))
	 (outcome (prob:sample (mdp:smdp-trans-dist m s a))))
    (values-list outcome))) 





