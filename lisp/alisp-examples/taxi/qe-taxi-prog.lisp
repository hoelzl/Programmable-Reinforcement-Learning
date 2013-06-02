(in-package #:common-lisp-user)

(defpackage #:qe-taxi-prog
  (:documentation "td-taxi-prog.lisp
Alisp program used in the qe-taxi domain

Functions
---------
qe-taxi-prog")

  (:use #:qe-taxi
	#:common-lisp
	#:utils
	#:alisp-prog)
  (:export #:qe-taxi-prog
	   #:N
	   #:S
	   #:E
	   #:W
	   #:P
	   #:D))

(in-package #:qe-taxi-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; access functions for state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-env-accessor taxi-pos taxi-env-state-pos)

(defun pass-src (i)
  (aref (taxi-env-state-src (env-state)) i))

(defun pass-dest (i)
  (aref (taxi-env-state-dest (env-state)) i))

(def-env-accessor num-pass taxi-env-state-n)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; partial program for Taxi domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav (loc)
  "nav LOC
Choose repeatedly among the N, E, S, and W actions until the taxi is in location LOC."
  (until (equal (taxi-pos) loc)
    (with-choice nav-choice (dir '(N E S W))
      (action nav-move dir))))

(defun get-pass (i)
  "get-pass I
Navigate to the source location of passenger I and pick them up."
  (call nav-src (nav (pass-src i)))
  (action pickup 'P))

(defun put-pass (i)
  "put-pass I
Navigate to the destination of passnger I and drop them off."
  (call nav-dest (nav (pass-dest i)))
  (action dropoff 'D))

(defun serve-next-pass ()
  "serve-next-pass
Choose the number of the next passenger, pick them up, drive them to theoir target location and
drop them off."
  (with-choice pass-choice (i (num-pass))
    (call (get-pass i))
    (call (put-pass i))))

(defun qe-taxi-prog ()
  "qe-taxi-prog
Serve all passengers."
  (loop
    (call (serve-next-pass))))





