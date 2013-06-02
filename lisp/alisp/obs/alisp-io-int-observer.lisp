;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp/obs/alisp-io-int-observer.lisp - defines <alisp-io-int-observer>, the 
;; kind of observer used by the io interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:alisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *hist* nil
  "When the IO interface is running, holds the trajectory of joint states seen during execution.
This provides a quick way to generate joint state objects - just use the IO interface to get
where you want, then stop and use the last item *hist*.  Set to a fresh vector on each run (so
the old *hist* can just be copied to some other place and won't get overwritten when a new run
happens).")


(defclass <alisp-io-int-observer> (<alisp-observer>)
  ((output-stream :initarg :stream
		  :reader outstream
		  :initform t
		  :type (or (eql t) stream))
   (choice-label-stack :initform nil
		       :accessor choice-label-stack))
  (:documentation "Class <alisp-io-int-observer>.
This class is used by the ALisp io-interface, and just prints out what happens in the ALisp
program to the stream."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-start-execution ((obs <alisp-io-int-observer>))
  (format (outstream obs) "~&Starting execution.")
  (setf *hist* (make-array 10 :adjustable t :fill-pointer 0)))

(defmethod inform-finish-execution ((obs <alisp-io-int-observer>))
  (format (outstream obs) "~&~%Execution complete."))

(defmethod inform-start-episode ((obs <alisp-io-int-observer>) s)
  (setf (choice-label-stack obs) nil)
  (format (outstream obs) "~&~%Beginning new episode in state ~&~a" s))

(defmethod inform-env-step ((obs <alisp-io-int-observer>) a r s2 term)
  (format (outstream obs)
          "~&~%Action ~A was done in the environment, yielding reward ~A.  New state is ~&~A. " 
          a r s2)
  (when term (format (outstream obs) "The episode has terminated.")))

(defmethod inform-part-prog-terminated ((obs <alisp-io-int-observer>) omega)
  (unless (y-or-n-p
           "~&~%The partial program has terminated at state ~A.~&Start a new episode?"
           omega)
    (error 'rlm-last-step-reached)))

(defmethod inform-alisp-step ((obs <alisp-io-int-observer>) omega u)
  (if (exit-state? omega)
      (format (outstream obs) "~&~%At exit state with label ~A."
              (second (js-pc omega)))
      (format (outstream obs) "~&~%At state with label ~A, chose ~A."
              (second (js-pc omega)) u)))

(defmethod inform-end-choice-block ((obs <alisp-io-int-observer>) omega)
  (declare (ignore omega))
  (format (outstream obs) "~&~%Leaving choice block."))
  



  
