;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; socket utils
;;
;; Utilities for dealing with sockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:common-lisp-user)

;; Package info
(defpackage #:socket-utils
  (:use #:common-lisp
	#:socket)
  (:export #:clear-all-input))

(in-package #:socket-utils)


(defun clear-all-input (s &optional (quiet nil))
  "clear-all-input S &optional (QUIET NIL)

Repeatedly call clear-input on socket s until listen returns false print a \".\" on each
attempt.  Setting quiet to true suppresses this."
(loop
      while (listen s)
      do (clear-input s)
	 (unless quiet (format t "."))))
