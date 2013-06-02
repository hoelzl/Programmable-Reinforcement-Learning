(in-package #:common-lisp-user)

(defpackage #:env-observer
  (:documentation "Package env-observer.  
Defines <env-observer>, a type of observer that prints out what happens in the environment to a
stream.

Exports
-------
<env-observer>
make-env-observer")
  (:use #:common-lisp
	#:rl-obs
	#:utils)
  (:export 
   #:<env-observer>
   #:make-env-observer))

(in-package #:env-observer)
  
(defclass <env-observer> (<rl-observer>)
  ((output-stream :type (or (eql t) stream)
		  :reader outstream :writer set-output-stream
                  :initarg :stream :initform t)
   (prompt-after-actions :type boolean
			 :accessor prompt?
			 :initarg :prompt?)
   (prompt-this-episode :type boolean
			:accessor prompt-this-ep)
   (eps :type fixnum
	:accessor eps
	:initform 0)
   (steps :type fixnum
	  :accessor steps
          :initform 0))
  (:documentation "Class <env-observer> (<rl-observer>)
The sole function of this class is to show what happens in the environment as a result of
running an ALisp program.  Create instances using make-env-observer"))




(defun make-env-observer (&optional (stream t) (prompt? nil))
  "make-env-observer &optional (OUTPUT-STREAM t) (PROMPT? nil)
Make an <env-observer>.  When PROMPT? is true, prompts user to press enter after each step."
  (make-instance '<env-observer> :stream stream :prompt? prompt?))

(defmethod inform-start-episode ((obs <env-observer>) s)
  (awhen (outstream obs)
    (format it "~&~%Environment reset to ~&~a" s))
  (setf (prompt-this-ep obs) t
	(steps obs) 0))
  
(defmethod inform-env-step ((obs <env-observer>) act rew to term)
  (awhen (outstream obs)
    (format it "~&Episode ~A Step ~A.  Did ~A, received ~A, and now in state ~&~A." 
            (eps obs) (steps obs) act rew to)
    (when (and (prompt? obs) (prompt-this-ep obs))
      (let ((c (prompt "~&Press enter to continue or \"skip\" to skip to end of this episode. ")))
        (when (equal c "skip") (setf (prompt-this-ep obs) nil)))))
  (incf (steps obs))
  (when term
    (awhen (outstream obs)
      (format it "~&Episode complete."))
    (incf (eps obs)))
  (values))

  
  
