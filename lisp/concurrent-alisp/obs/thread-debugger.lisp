;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calisp/obs/thread-debugger - defines <thread-debugger>
;; This is an observer that prints out thread-related happenings (whenever
;; a thread sleeps or wakes up) to standard output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:calisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <thread-debugger> (<calisp-debugging-observer>)
  ((output-stream :initarg :stream
		  :reader outstream
		  :initform *standard-output*
		  :type stream))
  (:documentation "Class <thread-debugger> (<calisp-debugging-observer>)
Subclass of <calisp-observer> that prints out thread-related events to a stream.

Initargs
STREAM: the stream to print to."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod inform-wait-action ((obs <thread-debugger>) omega id label)
  (declare (ignore omega))
  (force-format (outstream obs) "~&Thread ~a waiting for action at location ~a" id label))

(defmethod inform-wakeup ((obs <thread-debugger>) omega id)
  (declare (ignore omega))
  (force-format (outstream obs) "~&Thread ~a waking up" id))

(defmethod inform-main-thread-wait ((obs <thread-debugger>) omega)
  (declare (ignore omega))
  (force-format (outstream obs) "~&Control thread entering wait."))

(defmethod inform-main-thread-wakeup ((obs <thread-debugger>) omega)
  (declare (ignore omega))
  (force-format (outstream obs) "~&Control thread waking up."))

(defmethod inform-wait-choice ((obs <thread-debugger>) omega id label)
  (declare (ignore omega))
  (force-format (outstream obs) "~&Thread ~a waiting for choice at location ~a" id (second label)))

(defmethod inform-wait-effectors ((obs <thread-debugger>) omega id)
  (declare (ignore omega))
  (force-format (outstream obs) "~&Thread ~a waiting for effectors" id))
