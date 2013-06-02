(in-package #:common-lisp-user)

(defpackage #:reinforcement-learning
  (:documentation "Internal package for (flat) reinforcement learning.")
  (:nicknames #:rl)
  (:use #:common-lisp
	#:utils
	#:prob
        #:policy
        #:mdp)
  (:export #:*canonicalize-quickly*
           #:*clone-quickly*
           #:*hist*
           #:<alisp-approx-q-function>
           #:<learning-algorithm>
           #:<on-policy-learning-algorithm>
           #:<policy-learning-algorithm>
           #:<q-learning-algorithm>
           #:<rl-observer>
           #:<value-learning-algorithm>
           #:current-env-step
           #:current-episode-step
           #:debug-msg
           #:debug-str
           #:defmessage
           #:evaluate
           #:get-mdp
           #:get-mdp-hist
           #:get-policy
           #:get-policy-hist
           #:get-q-fn
           #:get-q-hist
           #:get-value-fn
           #:get-value-fn-hist
           #:hist
           #:inform-env-step
           #:inform-finish-execution
           #:inform-start-episode
           #:inform-start-execution
           #:io-interface
           #:knowledge-state
           #:learn
           #:learn-in-env
           #:make-q-learning-alg
           #:no-choice
           #:on-policy-learning
           #:random
           #:reset
           #:set-debug-str
           #:set-hist-collect))

(defpackage #:rl-user
  (:documentation "Package rl-user.
Package used when running rl algorithms in environments.

Exported symbols
==================

Running
-------
- learn
- on-policy-learning
- evaluate  
- io-interface
- random

General operations on learning algorithms
-----------------------------------------
- hist
- get-policy-hist
- get-q-hist
- get-value-fn-hist
- get-mdp-hist
- set-hist-collect
- reset
- debug-str
- set-debug-str

Making some common learning algorithms
--------------------------------------
- make-q-learning-alg


Related packages
================

Other observers 
---------------
- env-observer
- progress-printer
- q-learning
- gold-standard
- stat-gatherer
")
  (:use #:common-lisp
	#:package-utils
        #:rl)
  (:export #:debug-str
           #:evaluate
           #:get-mdp-hist
           #:get-policy-hist
           #:get-q-hist
           #:get-value-fn-hist
           #:hist
           #:io-interface
           #:learn
           #:make-q-learning-alg
           #:on-policy-learning
           #:random
           #:reset
           #:set-debug-str
           #:set-hist-collect))

(defpackage #:rl-observer
  (:nicknames #:rl-obs)
  (:documentation "Package rl-observer.
Used when making new observers/learning algorithms/stats gatherers.

Types
-----
<rl-observer>
<learning-algorithm>
<on-policy-learning-algorithm>
<q-learning-algorithm>
<policy-learning-algorithm>
<value-learning-algorithm>

Macro
-----
defmessage
debug-msg

Messages
--------
inform-start-execution
inform-finish-execution
inform-start-episode
inform-env-step

Learning algorithms must implement some subset of (depending on specific type)
------------------------------------------------------------------------------
reset
knowledge-state
get-q-fn (q-learning algorithms)
get-mdp (model-learning algorithms)
get-policy (policy-learning algorithms)
get-value-fn (value-learning algorithms)
make-choice (on-policy algorithms)

Functions for use by learning algorithms
----------------------------------------
current-env-step
current-episode-step

Other accessors for learning algorithms
---------------------------------------
debug-str
set-debug-str

")
  (:use #:common-lisp
	#:package-utils
        #:policy
        #:rl)
  (:export #:<learning-algorithm>
           #:<on-policy-learning-algorithm>
           #:<policy-learning-algorithm>
           #:<q-learning-algorithm>
           #:<rl-observer>
           #:<value-learning-algorithm>
           #:current-env-step
           #:current-episode-step
           #:debug-msg
           #:debug-str
           #:defmessage
           #:get-mdp
           #:get-policy
           #:get-policy-hist
           #:get-q-fn
           #:get-q-hist
           #:get-value-fn
           #:inform-env-step
           #:inform-finish-execution
           #:inform-start-episode
           #:inform-start-execution
           #:knowledge-state
           #:make-choice
           #:reset
           #:set-debug-str))
	     
