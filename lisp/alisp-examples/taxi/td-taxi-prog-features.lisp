(in-package #:common-lisp-user)

(defpackage #:td-taxi-prog-features
  (:documentation "Package for features for td taxi domain.

*featurizer*")
   
  (:use
   #:common-lisp
   #:alisp-features
   #:td-taxi-env
   #:td-taxi-flat-lfa
   #:td-taxi-prog
   #:gw
   #:utils)
  (:export
   #:*featurizer*))

(in-package #:td-taxi-prog-features)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions of the individual features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-feature taxi-dest (omega u)
  (stack-var-val 'loc t t))

(def-feature dropping-off-pass? (omega u)
  (stack-contains-frame 'put-pass))

(def-feature pos (omega u)
  (taxi-pos env-state))

(def-feature have-pass? (omega u)
  (eql (pass-loc env-state) 'in-taxi))

(def-feature pass-src (omega u)
  (pass-source env-state))

(def-feature pass-dst (omega u)
  (pass-dest env-state))

(def-feature source-dest-dist (omega u)
  (shortest-path-dist (env env-state) 
		      (pass-source env-state) 
		      (pass-dest env-state)))

(defparameter *featurizer*
  (make-3partq-featurizer
   ()
   (nav-choice
    (:qr-depends pos choice)
    (:qc-depends pos choice taxi-dest)
    (:qe-depends have-pass? dropping-off-pass? source-dest-dist))
   (nav-src
    (:qr-depends pos pass-src)
    (:qe-depends source-dest-dist))
   (nav-dest
    (:qr-depends pos pass-dst)
    (:qc-depends have-pass?)
    (:qe-depends have-pass? source-dest-dist))
   (task-choice
    (:qr-depends pos have-pass? pass-src pass-dst choice)
    (:qc-depends have-pass? pass-src pass-dst choice))))
