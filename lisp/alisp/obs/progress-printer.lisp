(in-package #:alisp)

(defclass <alisp-progress-printer> (<alisp-observer> progress-printer:<progress-printer>)
  ()
  (:documentation 
   "Class <alisp-progress-printer> (<alisp-observer> <progress-printer>)
See class <progress-printer> for details."))


(defun make-alisp-progress-printer (step-inc ep-inc &key (exec-notify nil) (stream t))
  "make-alisp-progress-printer STEP-INC EP-INC &key (exec-notify nil) (STREAM t)
See <progress-printer> for details."
  (make-instance '<alisp-progress-printer>
    :step-inc step-inc :episode-inc ep-inc :stream stream :exec-notify exec-notify))
