(defpackage :compounds
  (:use :common-lisp
	:elements)
  (:export #:pprint-compound
	   #:formula-weight
	   #:parse-compound
	   #:get-compound))

(in-package :compounds)

;; This program uses a special format for chemical formulae. The elements
;; are separated with spaces, like this:
;; C6 H12 06 => ((C 6) (H 12) (O 6))
;; (C O2)4 H2 => (((C (O 2)) 4) (H 2))
;; This makes it easier to read.

;; A component is composed of an element symbol or a compound enclosed in
;; parentheses, followed by a positive integer. It ends with whitespace
;; or the end of the string.

(defun element-number-pair-p (compound)
  "Return t if `compound' is a pair of an element and a number of that
element, such as (C 6)"
  (and (listp compound)
       (= (length compound)
	  2)
       (numberp (second compound))))

(defun pprint-compound (compound &key (stream *standard-output*) parens)
  "Pretty print a compound, putting parens around it if `parens' is t.
This is so ugly that I've come to despise it, but it works. Don't touch it
unless you want it to shatter into a million little pieces."
  (when parens
    (write-char #\( stream))
  (if (listp compound)
      (let (elements-list)
	;; If the compound is just a simple element designator, add it to the
	;; elements list to queue it for printing
	(if (typep compound '(or string symbol number))
	    (push (element-symbol (get-element compound))
		  elements-list)
	    (if (element-number-pair-p compound)
		(push (format nil "~A~A"
			      (let ((chemical (first compound)))
				(if (listp chemical)
				    (with-output-to-string (s)
				      (pprint-compound chemical
						       :stream stream
						       :parens t))
				    (element-symbol (get-element chemical))))
			      (second compound))
		      elements-list)
		(dolist (component compound)
		  (push (with-output-to-string (s)
			  (pprint-compound component :stream s))
			elements-list))))
	(if elements-list
	    (format stream "~{~A~^ ~}" (nreverse elements-list))))
      (princ (element-symbol (get-element compound)) stream))
  (when parens
    (write-char #\) stream)))

(defun formula-weight (compound)
  "The number of grams per mole of a compound"
  (if (element-number-pair-p compound)
      (* (element-atomic-weight (get-element (first compound)))
	 (second compound))
      (if (listp compound)
	  (apply #'+ (mapcar #'formula-weight compound))
	  (element-atomic-weight (get-element compound)))))

(defun parse-compound (string)
  "Parse a compound string, such as C6 H12 O6"
  (compound-parsing:parse-compound string))

(defun get-compound (compound)
  "Return a compound designator given either a compound designator or a
string in compound syntax"
  (typecase compound
    (list compound)
    (string (parse-compound compound))))

;(pprint-compound '(((C (O 2)) 4) (H 2)))