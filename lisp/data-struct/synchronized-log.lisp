;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; synchronized-log.lisp - maintains a thread-safe log of events, for debugging multithreaded
;; programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:synchronized-log
  (:nicknames #:slog)
  (:use #:common-lisp
	#:utils
	#:mp)
  (:export 
   #:<synchronized-log>
   #:reset
   #:add
   #:set-stream
   #:get-log
   #:with-slogfile
   #:make-filter
   #:set-include-thread
   #:watch
   #:filter))
   
(in-package #:slog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <synchronized-log> ()
  ((lock :type process-lock
	 :initform (make-process-lock :name "Synchronized log lock")
	 :reader lock)
   (log :type vector
	:writer set-log
	:reader get-log)
   (str :type stream
	:writer set-stream
	:initform nil
	:reader get-stream)
   (include-thread :type boolean
		   :initform t
		   :writer set-include-thread 
		   :documentation "Whether to include thread name in log messages"
		   :reader include-thread)
   )
  (:documentation "Class <synchronized-log>
Use reset and add to modify the log in a thread-safe way.  The accessor method get-log will
return the log (but this is not thread-safe.

Initargs
:str - if non-nil, a stream to which each entry will also be sent when it is added "))


(defmethod initialize-instance :after ((sl <synchronized-log>) &rest args)
  (reset sl))

(defparameter *slog-init-length* 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; synchronized methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((sl <synchronized-log>) &aux (lock (lock sl)))
  "reset SLOG.  Remove all entries from the synchronized log."
  (with-process-lock (lock)
    (set-log (make-array *slog-init-length* :adjustable t :fill-pointer 0) sl)
    (let ((s (get-stream sl)))
      (when s (unless (open-stream-p s) (set-stream nil sl))))))

(defmethod add ((sl <synchronized-log>) &rest args &aux (lock (lock sl)))
  "add SLOG ENTRY
Add a new entry to the synchronized log and note which thread added it.  Alternate syntax.  add
SLOG &rest ARGS with multiple arguments.  Treat ARGS as arguments to (format nil ...)"

  (with-process-lock (lock)
    (let ((new-entry 
            (let ((item (if (= (length args) 1) 
                            (first args) 
                            (apply #'format nil args))))
              (if (include-thread sl)
                  (cons (process-name *current-process*) item)
                  item))))
      (circular-push new-entry (get-log sl))
      (awhen (get-stream sl)
        (pprint new-entry it)
        (force-output it)))))
    


(defun watch (sl &optional (int 2) (filter (constantly t)))
  "watch SLOG &optional (INT 2)
Every INT seconds, check if the log has been updated and if so, print what's been added.  TODO:
purely temporary, should do this in a cleaner way with condition variables and in a more general
setting with a listener class or something"
  (loop
      with len = 0
      with lock = (lock sl)
      with log = (get-log sl)
		 
      do (with-process-lock (lock)
	   (when (> (length log) len)
	     (format t "~&$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
	     (map nil (lambda (x) (when (funcall filter x) (pprint x))) (subseq log len))
	     (setf len (length log))))
	 (sleep int)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nonsychronized methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-filter (&key thread thread-not entry)
  "make-filter &key THREAD THREAD-NOT ENTRY

Return a predicate that is true of (thread . item-desc) iff
- The thread must satisfy the predicate defined by THREAD if it is non-nil, or the complement of
  the predicate defined by THREAD-NOT if it is non-nil.
- The item description must satisfy the predicate defined by ENTRY.

The 'predicate defined by X' is defined to include all Y such that:
- if X is a string, Y contains X
- If X is a list, Y contains one of the elements of X
- If X is a function, X(Y) = true"
  (assert (not (and thread thread-not)) nil "thread and thread-not can't both be non-nil")
  (let* ((thread-pred (if thread (pred-defined-by thread)
                          (if thread-not (complement (pred-defined-by thread-not))
                              (constantly t))))
	 (entry-pred (if entry (pred-defined-by entry) (constantly t))))
    (lambda (x) (and (funcall thread-pred (car x)) (funcall entry-pred (cdr x))))))
  

(defgeneric filter (l &key thread thread-not entry)
  (:documentation "filter L &key THREAD THREAD-NOT ENTRY
L is a synchronized log.  Return those elements of L that satisfy the predicate generated by
calling make-filter on THREAD, THREAD-NOT, and ENTRY.")
  (:method ((l sequence) &rest args)
    (let ((f (complement (apply #'make-filter args))))
      (remove-if f l)))
  (:method ((l <synchronized-log>) &rest args)
    (apply #'filter (get-log l) args)))


(defmethod pred-defined-by ((x t))
  (lambda (y) (search x y)))

(defmethod pred-defined-by ((x list))
  (lambda (y)
    (some (lambda (z) (search z y)) x)))

(defmethod pred-defined-by ((x function))
  (lambda (y)
    (funcall x y)))
			    
(defmethod print-object ((x <synchronized-log>) str)
  (pprint (get-log x) str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-slogfile ((l filename) &body body)
  "with-logfile (LOG FILENAME) &body BODY
Run the code in body, while sending the output of synchronized log LOG to FILENAME.  The file
will be overwritten if it already exists."
  (with-gensyms (str old-val)
    `(with-open-file (,str ,filename :direction :output :if-exists :supersede)
       (let ((,old-val (get-stream ,l)))
	 (set-stream ,str ,l)
	 ,@body
	 (set-stream ,old-val ,l)))))
