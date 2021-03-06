;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alisp-state.lisp
;; Defines various components of the joint state of an ALisp program running in an environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:alisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; joint state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type def
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype joint-state-type ()
  "Type joint-state-type
Specifies what the RLM is currently doing with the state:
NIL:              The state is freshly created but not yet used by the RLM(?)
ACTION:           The RLM is about to or in the process of evaluating an action
                  definition (i.e., cloning the state before executing an action, checking
                  whether the RLM is at a terminal state, evaluating DO-ACTION).
ACTION-EXIT:      The RLM has completed evaluating DO-ACTION.
CALL:             The RLM is about to or in in the process of evaluating a call (i.e, choosing
                  values for unspecified variables, cloning the state before executing a call,
                  filling unspecified arguments, evaluating the call code.
CALL-EXIT:        The RLM has completed a call.
CHOOSE:           The RLM is about to or in the process of evaluating a CHOOSE form.
CHOOSE-EXIT:      The RLM has completed a CHOOSE form.
INTERNAL:         The RLM is performing internal cleanup or bookkeeping actions.
WITH-CHOICE:      The RLM is about to or in the process of evaluating a WITH-CHOICE form.
WITH-CHOICE-EXIT: The RLM has completed a WITH-CHOICE form.
"
  '(or null
    (member action action-exit call call-exit choose choose-exit internal 
     with-choice with-choice-exit)))

(defstruct (joint-state (:conc-name js-))
  pc
  next-frame
  env-state
  global
  stack
  choices
  (type nil :type joint-state-type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; canonicalize and clone functions for joint states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *clone-quickly* nil 
  "If the current partial program never changes local stack frames (using mem) after creation,
and if choice set objects never change, set this to t for improved performance.")

(defvar *canonicalize-quickly* nil 
  "If the current partial program memory items are all descendable by #'equalp (i.e. lists,
vectors, numbers, symbols, or strings) then set this to t for improved performance.")

(defmethod clone ((omega joint-state))
  (make-joint-state :pc (js-pc omega) :next-frame (clone (js-next-frame omega))
		    :env-state (js-env-state omega) :global (clone (js-global omega))
		    :stack (if *clone-quickly* (js-stack omega) (clone (js-stack omega)))
		    :choices (if *clone-quickly* (js-choices omega) (clone (js-choices omega))) 
		    :type (js-type omega)))

(defmethod canonicalize ((omega joint-state))
  (if *canonicalize-quickly*
      (list (js-pc omega)
            (js-next-frame omega)
            (canonicalize (js-env-state omega))
            (js-global omega)
            (js-stack omega)
            (canonicalize (js-choices omega))
            (js-type omega))
      (list (js-pc omega)
            (canonicalize (js-next-frame omega))
            (canonicalize (js-env-state omega))
            (canonicalize (js-global omega))
            (canonicalize (js-stack omega))
            (canonicalize (js-choices omega))
            (js-type omega))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other functions on joint states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-next-frame-to-stack (omega)
  "move-next-frame-to-stack OMEGA
Moves next-frame of OMEGA (which must be non-nil) to the top of the stack and sets the
next-frame field of OMEGA to nil."
  (let ((next-frame (js-next-frame omega)))
    (assert next-frame nil "Attempted to move nil frame to stack")
    (push next-frame (js-stack omega)))
  (setf (js-next-frame omega) 'not-at-choice))

(defun action-state? (omega)
  (eq (js-type omega) 'action))

(defun exit-state? (omega)
  (member (js-type omega) '(action-exit call-exit with-choice-exit choose-exit)))

(defmethod print-object ((omega joint-state) str)
  (print-unreadable-object (omega str :type t)
    (format str "~A-state, PC: ~A, choice-set: ~A~&Env state: ~A~&Stack: ~A"
            (string-downcase (symbol-name (js-type omega)))
            (let ((pc (js-pc omega)))
              (if (listp pc) (second pc) pc))
            (js-choices omega)
            (js-env-state omega) 
            (or (js-stack omega) "empty") )))


(defun stack-var-val (omega name &optional (next t) (check-not-null nil))
  "stack-var-val OMEGA NAME &optional (INCLUDE-NEXT-FRAME t) (CHECK-NOT-NULL nil)

Look for a variable in one of the stack frames and, if INCLUDE-NEXT_FRAME is t, the next frame.
If such a variable is found, return its value and t.  Otherwise, if CHECK-NOT-NULL is true,
assert.  Otherwise, return nil and nil."
  
  ;; helper function that looks for variable in a particular frame and returns
  ;; from top-level function if it succeeds
  (flet ((find-in-frame (f)
	   (dolist (b (frame-entries f))
	     (when (eq (car b) name)
	       (return-from stack-var-val
		 (values (cdr b) t))))))
    
    ;; main function - first check the stack
    (mapc #'find-in-frame (js-stack omega))
    
    ;; then check the next frame if necessary
    (when next (find-in-frame (js-next-frame omega)))
    
    (assert (not check-not-null) () "Unexpected nil value.")
    
    ;; if we still haven't returned, return nil
    (values nil nil)))

(defun stack-contains-frame (omega name)
  "stack-contains-frame OMEGA NAME
Does the stack of OMEGA contains a frame called NAME?"
  (to-boolean (member name (js-stack omega) :key #'frame-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (frame (:constructor make-frame (name &optional (label nil))) (:type list))
  ;; don't change it away from type list (clone)
  name
  label
  (entries nil))

(defun set-frame-var-val (frame var val &optional (already-exists nil exists-supplied))
  "set-frame-var-val FRAME VAR VAL &optional ALREADY-EXISTS
Set value of VAR in FRAME to VAL.  If ALREADY-EXISTS is supplied, then the existence or
nonexistence of an entry for the given VAR is verified in advance and an assertion happens if
the expectations fail."

  (let* ((entries (frame-entries frame))
	 (entry (assoc var (frame-entries frame))))
    (when exists-supplied
      (if already-exists
	  (assert entry ()
                  #.(str "Contrary to expectations, entry for ~A does not exist in entries "
                         "~A of frame ~A.")
		  var entries frame)
          (assert (not entry) ()
                  "Contrary to expectations, entry ~A does exist in entries ~a of frame ~A."
                  entry entries frame)))
    (if entry
	(setf (cdr entry) val)
        (push (cons var val) (frame-entries frame)))))

(defun get-frame-var-val (frame var)
  "get-frame-var-val FRAME VAR
Return value of VAR in FRAME.  VAR must already exist."
  (let ((entries (frame-entries frame))
	(entry (assoc var (frame-entries frame))))
    (assert entry ()
            "Entry for ~A does not exist among entries ~A of frame ~A."
            var entries frame)
    (cdr entry)))

;; frames are just lists, so same, clone, and canonicalize already defined

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; program counters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (program-counter (:type list))
  containing-subroutine
  label)
