;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rl-functions/policy/greedy-policy.lisp
;; Defines <greedy-policy> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:policy)

(defclass <greedy-policy> (<policy>)
  ((q-function :type q-fn:<q-function>
	       :reader q-function
	       :initarg :q-function
               :initform (required-initarg :q-function))
   (random-choice :initform t
		  :reader random-choice
		  :initarg :random-choice)
   (dist-cache :initform (make-hash-table :test 'equalp)
               :reader dist-cache))
  ;; TODO: The documentation for RANDOM-CHOICE claimed that when an unknown-state-action is
  ;; encountered the behavior is the same as for unknown-state, i.e., the function is applied to
  ;; the state.  This does not seem to be the case in the code.  Check whether this is the
  ;; intended behavior. --tc
  (:documentation "Class <greedy-policy> (<policy>)

Initargs
:q-function -    make decisions by maximizing this q-function.
:random-choice - This can be either 
                 1) a function, such that if an unknown-state condition is encountered, then a
                 choice is instead made by applying this function to the current state, and
                 choosing randomly from the resulting state.If an unknown-state-action is
                 encountered in this case, an action is chosen randomly from the set of
                 available actions.
                 2) nil, in which case errors are passed up to the caller
                 3) t (the default), in which case unknown-state errors are passed up, but
                 unknown-state-action errors are handled by choosing randomly from the known set
                 of available actions"))

(defmethod make-choice ((d <greedy-policy>) omega)
  (handler-bind ((q-fn:unknown-state-action
                   #'(lambda (c)
                       (when (random-choice d)
                         (q-fn:choose-randomly c))))
		 (q-fn:unknown-state
                   #'(lambda (c)
                       (declare (ignore c))
                       (awhen (random-choice d)
                         (when (functionp it)
                           (use-value (prob:sample-uniformly (funcall it omega))))))))
    (q-fn:best-choice (q-function d) omega)))


;;; Added this to better support the <policy> protocol. --tc
(defmethod choice-dist ((d <greedy-policy>) omega)
  (let* ((choice (make-choice d omega))
         (cached-dist (gethash choice (dist-cache d) nil)))
    (or cached-dist
        (let ((dist (prob:make-multinomial-dist #(1.0) (list choice))))
          (setf (gethash choice (dist-cache d)) dist)
          dist))))

(defmethod clone ((pol <greedy-policy>))
  (check-exact-class pol '<greedy-policy>)
  (make-instance '<greedy-policy>
    :q-function (clone (q-function pol)) :random-choice (random-choice pol)))

(defmethod print-object ((pol <greedy-policy>) str)
  (if *print-readably*
      (progn
	(check-exact-class pol '<greedy-policy>)
	(format str "#.(make-instance 'policy:<greedy-policy> :q-function ")
	(write (q-function pol) :stream str)
	(format str " :random-choice ~W)" (random-choice pol)))
      (call-next-method)))

		
