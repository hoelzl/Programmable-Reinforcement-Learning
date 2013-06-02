(in-package #:common-lisp-user)

(defpackage #:progress-printer
  (:documentation "Package progress printer.
Defines <progress-printer>, an observer that prints progress of learning to a stream.

Exports
-------
<progress-printer>
make-progress-printer")
  (:use #:common-lisp
	#:rl-obs
	#:utils)
  
  (:export #:<progress-printer>
           #:make-progress-printer
	   #:finish-episode))

(in-package #:progress-printer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <progress-printer> (<rl-observer>)
  ((elapsed-steps :type fixnum :initform 0 :accessor elapsed-steps)
   (elapsed-episodes :type fixnum :initform 0 :accessor elapsed-episodes)
   (step-print-inc :initarg :step-inc :initform nil :reader step-inc)
   (episode-print-inc :initarg :episode-inc :initform nil :reader episode-inc)
   (notify-start-finish-execution :initarg :exec-notify :initform nil :reader exec-notify)
   (output-stream :type (or (eql t) stream) :initarg :stream :reader outstream :initform t))
  (:documentation "Class <progress-printer> (<observer>) 
Class that is used to print progress of run to a stream.

Initargs
:stream -      the stream to print to.  t by default.
:episode-inc.  nil by default.
:step-inc.     nil by default.
:exec-notify.  Whether to print message at start and finish of execution.  Nil by default.

If episode-inc is non-nil, then every time the number of finished episodes is divisible by
episode-inc, print progress to stream on a newline.
If step-print-inc is non-nil, then every time the number of elapsed steps is divisible by
step-inc, print a '.' to the stream.  The number of elapsed steps is initialized to 0 when the
run begins and incremented on every env-step."))


(defun make-progress-printer (step-inc ep-inc &key (exec-notify nil) (stream t))
  "make-progress-printer STEP-INC EP-INC &key (exec-notify nil) (STREAM t)
See <progress-printer> for details."
  (make-instance '<progress-printer>
    :step-inc step-inc :episode-inc ep-inc :stream stream :exec-notify exec-notify))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod inform-start-execution ((obs <progress-printer>))
  (setf (elapsed-episodes obs) 0
	(elapsed-steps obs) 0)
  (when (exec-notify obs)
    (force-format (outstream obs) "~&Execution begun~%~%"))
  (when (episode-inc obs)
    (force-format (outstream obs) "~&Episode 0")))

(defmethod inform-finish-execution ((obs <progress-printer>))
  (when (exec-notify obs)
    (force-format (outstream obs) "~&Execution finished.")))

(defun finish-episode (obs)
  (let ((inc (episode-inc obs))
	(eps (incf (elapsed-episodes obs))))
    (when (and inc (eql 0 (mod eps inc)))
      (force-format (outstream obs) "~&Episode ~a" eps))))

(defmethod inform-env-step ((obs <progress-printer>) a r s2 term)
  (declare (ignore s2 a r))
  (let ((inc (step-inc obs))
	(steps (incf (elapsed-steps obs))))
    (when (and inc (eql 0 (mod steps inc)))
      (force-format (outstream obs) ".")))
  (when term (finish-episode obs)))
      
