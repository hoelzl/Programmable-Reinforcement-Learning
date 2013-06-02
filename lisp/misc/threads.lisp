;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; threads.lisp
;;
;; Functions for dealing with the Allegro multiprocessing system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:common-lisp-user)

;; Package info
(defpackage #:threads
  (:use #:common-lisp
	#:mp
	#:utils)
  (:export #:establish-bindings
	   #:kill-proc
	   
	   ;; #:process-reset ; Seems to be unused --tc
	   #:process-name        ; -> bt:thread-name
	   #:process-preset      ; ~ pass function to make-thread
	   #:process-enable      ; ~ used for notification and condition variables
	   #:process-wait        ; -> bt:join-thread
	   #:make-process-lock   ; -> bt:make-lock or bt:make-recursive-lock
	   #:process-lock        ; -> bt:acquire-lock
	   #:process-unlock      ; -> bt:release-lock
	   #:process-lock-locker ; ~ need to maintain our own table?
	   #:make-process        ; -> bt:make-thread
	   #:process-kill        ; -> bt:destroy-thread
	   #:*current-process*   ; -> (bt:current-thread)
	   #:*all-processes*     ; -> (bt:all-threads)
	   #:with-process-lock   ; -> bt:with-lock-held or bt:with-recursive-lock-held
	   ))

(in-package #:threads)

;; kill-proc
;; kill all processes with given names
;;
;; names : list of names
(defmethod kill-proc ((names list))
  (loop
      for p in *all-processes*
      if (member (process-name p) names
                 :test #'(lambda (x y) (search y x)))
      do (process-kill p)))


;; kill-proc
;; special case of the above when there's just a single process
;;
;; name : string
(defmethod kill-proc ((name string))
  (kill-proc (list name)))



;; establish-bindings
;; used to set dynamic bindings when running a new process.
;; 
;; symbol-list : list of symbols
;; value-list : list of values
;; fn : function
;; args : arguments
;;
;; See the Allegro online documentation, multiprocessing section 7 for how to use
;; this.
(defun establish-bindings (symbol-list value-list fn args)
  (progv symbol-list value-list (apply fn args)))
