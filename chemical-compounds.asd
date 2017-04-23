;; -*- Lisp -*-

(defpackage #:chemical-compounds-system
  (:use #:common-lisp #:asdf))

(in-package #:chemical-compounds-system)

(defsystem chemical-compounds
  :author "Peter Scott"
  :licence "LLGPL"
  :version "1.0.1"
  :components ((:file "parsing")
	       (:file "compounds" :depends-on ("parsing")))
  :depends-on (periodic-table))