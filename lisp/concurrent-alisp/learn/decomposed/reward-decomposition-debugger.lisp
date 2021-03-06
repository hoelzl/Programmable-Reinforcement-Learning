(in-package #:common-lisp-user)

(defpackage #:reward-decomposition-debugger
  (:documentation "Package reward-decomposition-debugger (rew-dec-deb)

Types
-----
<reward-decomposition-debugger>
")
  (:export
   #:<reward-decomposition-debugger>
   )
  (:nicknames #:rew-dec-deb)
  (:use
   #:common-lisp
   #:utils
   #:calisp-obs))
	    
(in-package #:rew-dec-deb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <reward-decomposition-debugger> (<calisp-observer>)
  ((prev-omega :accessor prev-omega)
   (prev-s :accessor prev-s)
   (reward-decomposer :type function :reader reward-decomposer
                      :initarg :reward-decomposer
                      :initform (required-initarg :reward-decomposer))
   (output-stream :type (or (eql t) stream) :initarg :stream :reader outstream :initform t)
   )
  (:documentation "Class <reward-decomposition-debugger> (<calisp-observer>)

An observer used solely for debugging reward decomposition functions.  Simply prints out the
reward decomposition at each step to the output stream.

Initargs

REWARD-DECOMPOSER: A function that takes in five arguments : omega, s, a, r, s2, and returns a
                   reward decomposition, i.e., an association list from the threads of omega to
                   real-valued rewards which add up to r.
STREAM:            The output stream.  Defaults to t."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; responses to messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-start-episode ((alg <reward-decomposition-debugger>) s)
  (setf (prev-s alg) s)
  (unbind-slot alg 'prev-omega))

(defmethod inform-calisp-step ((alg <reward-decomposition-debugger>) omega u)
  (declare (ignore u))
  (setf (prev-omega alg) omega))


(defmethod inform-env-step ((alg <reward-decomposition-debugger>) a r s2 term)
  (declare (ignore term))
  (when (slot-boundp alg 'prev-omega)
    (let ((rewards (funcall (reward-decomposer alg) (prev-omega alg) (prev-s alg) a r s2)))
      (assert (= r (reduce #'+ rewards :key #'cdr)) ()
              #.(str "Rewards ~A don't add up to ~A in reward decomposition for "
                      "doing ~A in ~A and getting to ~A.")
              rewards r a (prev-s alg) s2)
      (format (outstream alg) "~&Total reward ~A decomposed as ~A" r rewards))))






