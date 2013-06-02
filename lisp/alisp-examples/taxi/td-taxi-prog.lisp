(in-package #:common-lisp-user)

(defpackage #:td-taxi-prog
  (:documentation "td-taxi-prog.lisp -  Alisp program for Dietterrich's taxi domain
Functions
---------
td-taxi-prog

Symbols used as labels for choices
----------------------------------
N
S
E
W
P
D
nav-src
nav-src-exit
nav-dest
nav-dest-exit
get-pass
put-pass
nav-choice
nav-choice-exit
task-choice-exit

")

  (:use #:td-taxi-env
	#:common-lisp
	#:utils
	#:alisp-prog)
  (:export #:td-taxi-prog
	   #:N
	   #:S
	   #:E
	   #:W
	   #:P
	   #:D
	   #:loc
	   #:task-choice
	   #:task-choice-exit
	   #:nav
	   #:nav-choice
	   #:nav-choice-exit
	   #:nav-src
	   #:nav-src-exit
	   #:nav-dest
	   #:nav-dest-exit
	   #:get-pass
	   #:put-pass))

  


(in-package #:td-taxi-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; access functions for state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-env-accessor passenger-loc pass-loc)
(def-env-accessor passenger-src pass-source)
(def-env-accessor passenger-dest pass-dest)
(def-env-accessor pos taxi-pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; partial program for Taxi domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nav (loc)
  "nav LOC
Choose repeatedly among the N, E, S, and W actions until the taxi is in location LOC."
  (until (equal (pos) loc)
    (with-choice nav-choice (dir '(N E S W))
      (action nav-move dir))))

(defun get-pass ()
  "get-pass
Navigate to the source location of the passenger and pick them up."
  (call nav-src (nav (passenger-src)))
  (action pickup 'P))

(defun put-pass ()
  "put-pass
Navigate to the destination of the passenger and drop them off."
  (call nav-dest (nav (passenger-dest)))
  (action dropoff 'D))

(defun td-taxi-prog ()
  "td-taxi-prog
Repeatedly choose between picking the passenger up and dropping them off."
  (loop 
    (choose task-choice
	    (call (get-pass))
	    (call (put-pass)))))



