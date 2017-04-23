(defpackage :compound-parsing
  (:use :common-lisp)
  (:export #:parse-compound))

(in-package :compound-parsing)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct (meta
	      (:print-function
	       (lambda (m s d &aux (char (meta-char m)) (form (meta-form m)))
		 (declare (ignore d))
		 (ecase char
		   ((#\@ #\! #\$) (format s "~A~A" char form))
		   (#\[ (format s "[~{~A~^ ~}]" form))
		   (#\{ (format s "{~{~A~^ ~}}" form))))))
    char
    form)

  (defun compileit (x)
    (typecase x
      (meta
       (ecase (meta-char x)
	 (#\! (meta-form x))
	 (#\[ `(and ,@(mapcar #'compileit (meta-form x))))
	 (#\{ `(or ,@(mapcar #'compileit (meta-form x))))
	 (#\$ `(not (do ()
			((not ,(compileit (meta-form x)))))))
	 (#\@ (let ((f (meta-form x)))
		`(match-type ,(car f)
			     ,(cadr f))))))
      (t `(match ,x))))

  (defmacro matchit (x) (compileit x))

  (defun meta-reader (s c) (make-meta :char c :form (read s)))

  (mapc #'(lambda (c) (set-macro-character c #'meta-reader)) '(#\@ #\$ #\!))

  (set-macro-character #\[
		       #'(lambda (s c) (make-meta :char c :form (read-delimited-list #\] s t))))

  (set-macro-character #\{
		       #'(lambda (s c) (make-meta :char c :form (read-delimited-list #\} s t))))

  (mapc #'(lambda (c) (set-macro-character c (get-macro-character #\) nil)))
	'(#\] #\})))

(defmacro match (x)
  (etypecase x
    (character
     `(when (and (< index end)
		 (eql (char string index)
		      ',x))
	(incf index)))
    (string
     `(let ((old-index index))	  ; 'old-index' is a lexical variable.
	(or (and ,@(map 'list
			#'(lambda (c)
			    `(match ,c))
			x))
	    (progn (setq index old-index)
		   nil))))))

(defmacro match-type (x v)
  `(when (and (< index end)
	      (typep (char string index) ',x))
     (setq ,v (char string index))
     (incf index)))

; Example: Parsing Integers

; {a0 .. an}   Alternatives
; [s0 .. sn]   Sequence
; !sexpr       Lisp expression
; @expr        Once
; $expr        Zero or more

(deftype digit ()
  '(satisfies digit-char-p))

(defun ctoi (d)
  (- (char-code d)
     #.(char-code #\0)))

(defvar *meta-debug* nil
  "Should META output debugging information?")

(defmacro defmeta (name other-args &body body)
  `(defun ,name (string &optional (index 0) (end (length string))
			,@other-args)
     (declare (simple-base-string string)
	      (fixnum index end))
     ,@body))

(defmacro meta-labels (functions &body body)
  (let* ((old-index (gensym "old-index-"))
	 (labels-list
	  (loop for fun in functions
		collect (destructuring-bind (name extra-args &body body)
			    fun
			  `(,name (&aux (,old-index index) ,@extra-args)
				  (when *meta-debug*
				    (format t
					    "~&~S : ~S"
					    ',name
					    (subseq string index)))
				  (or (progn ,@body)
				      (progn (setf index ,old-index)
					     nil)))))))
    `(labels ,labels-list
       ,@body)))

(defmeta parse-int (&aux (s +1) d (n 0))
  ;; Lexical 'string', 'index', and 'end', as required by matchit.
  (and
   (matchit
    [{#\+ [#\- !(setq s -1)] []}
	  @(digit d) !(setq n (ctoi d))
	  $[@(digit d) !(setq n (+ (* n 10) (ctoi d)))]])
   (* s n)))

;(parse-int "12345")

;; Hop on Pop
;;;;;;;;;;;;;

(deftype space-char ()
  '(member #\ ))

(deftype nonspace-char ()
  '(member #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z 
	   #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(defmeta parse-x-and-y (&aux verb noun c)
  (meta-labels ((verb ()
		      (matchit [@(nonspace-char c) !(push c verb)
				$[@(nonspace-char c) !(push c verb)]]))
		(noun ()
		      (matchit [@(nonspace-char c) !(push c noun)
				$[@(nonspace-char c) !(push c noun)]]))
		(whitespace ()
			    (matchit [@(space-char c)
				      $[@(space-char c)]])))
     ;; Main body
     (matchit
      [!(verb)
       !(whitespace)
       "on"
       !(whitespace)
       !(noun)]))
  (list (coerce (nreverse verb)
		'string)
	(coerce (nreverse noun)
		'string)))

;(parse-x-and-y "hop on pop")

;; Query strings
;;;;;;;;;;;;;;;;

(deftype number-char ()
  '(or (satisfies digit-char-p)
       (member #\. #\/)))

(deftype non-rparen-char ()
  '(and (not (member #\)))
	(satisfies characterp)))

(defmacro with-string-accumulation (strings &body body)
  "Given a list of symbols, create character output streams bound to the
symbols given in `strings' with -STREAM appended, execute `body' in this
environment, and SETF all the symbol-values of the symbols in `strings'
to the values of their respective string output streams.

It also defines a local function FORCE-STRING-ACCUMULATION which will
do the SETFing when it is called. It is called automatically at the end
of the block, but you can call it earlier, as many times as you like.
It clears the string streams, so be careful when using it.

The utility of this macro may not be readily apparent, but is can be very
handy in META parsers which need to accumulate characters into strings."
  (let ((let-body (loop for string in strings
			collect `(,(intern (format nil "~S-STREAM" string))
				  (make-string-output-stream))))
	(unwind-protect-body
	 (loop for string in strings
	       collect `(close ,(intern (format nil "~S-STREAM"
						string)))))
	(setf-body
	 (loop for string in strings
	       collect `(setf ,string
			      (get-output-stream-string
			       ,(intern (format nil "~S-STREAM"
						string)))))))
    `(let ,let-body
       (labels ((force-string-accumulation ()
		  ,@setf-body))
	 (unwind-protect (progn ,@body)
	   ,@unwind-protect-body)
	 (force-string-accumulation)))))

(defmeta parse-query-string (&aux amount unit element
				  base-triple
				  conditions
				  c)
  (with-string-accumulation (amount unit element)
    (meta-labels ((amount ()
		    (matchit
		     [@(number-char c) !(princ c amount-stream)
		       $[@(number-char c) !(princ c amount-stream)]]))
		  (unit ()
		    (matchit
		     [@(nonspace-char c) !(princ c unit-stream)
		       $[@(nonspace-char c) !(princ c unit-stream)]]))
		  (element ()
		    (matchit
		     [@(non-rparen-char c) !(princ c element-stream)
		       ${[@(non-rparen-char c) !(princ c element-stream)]
		         [#\) @(digit c)
			  !(format element-stream
				   ")~A" c)]}
		       ]))
		  (whitespace ()
		    (matchit [@(space-char c)
			       $[@(space-char c)]]))
		  (at/and ()
		    (matchit [#\a
			      {#\t [#\n #\d]}])))
      (matchit
       [!(amount)
	!(whitespace)
	!(unit)
	!(whitespace)
	!(element)

	;; Set the base triple
	!(force-string-accumulation)
	!(setf base-triple
	       (list amount unit element))
	
	$[!(whitespace)
	  !(at/and)
	  !(amount)
	  !(whitespace)
	  !(unit)

	  !(force-string-accumulation)
	  !(push (list amount unit)
		 conditions)]])))
  (list base-triple conditions))


;(parse-query-string "12.2 g (H 2) at 12 atm")

;; Compound parsing
;;;;;;;;;;;;;;;;;;;

;; This program uses a special format for chemical formulae. The elements
;; are separated with spaces, like this:
;; C6 H12 06 => ((C 6) (H 12) (O 6))
;; (C O2)4 H2 => (((C (O 2)) 4) (H 2))
;; This makes it easier to read.

;; A component is composed of an element symbol or a compound enclosed in
;; parentheses, followed by a positive integer. It ends with whitespace
;; or the end of the string.

(defun strings->element-list (element number)
  "Convert an element (which may be a string or a compound designator) and
a number given as a string into a proper compound designator."
  (let ((out-element (if (stringp element)
			 (intern element)
		       element))
	(parsed-number (parse-integer number)))
    (if (= parsed-number 1)
	out-element      
      (list (if (stringp element)
		(intern element)
	      element)
	    parsed-number))))

(defun end-of-string-p (string index)
  "Is index above the maximum index allowed for a given string?"
  (<= (length string)
      index))

(defmeta parse-compound (&aux component-list
			      component-list-stack
			      element-symbol
			      number
			      c)
  (with-string-accumulation (element-symbol number)
    (meta-labels ((element-symbol ()
		    (matchit [@(nonspace-char c)
			      !(princ c element-symbol-stream)
			      $[@(nonspace-char c)
				!(princ c element-symbol-stream)]]))
		  (number ()
		    (matchit {[@(number-char c)
			       !(princ c number-stream)
			       $[@(number-char c) !(princ c number-stream)]]
			      [{@(space-char c) ; Ugly hack
				#\)
				!(end-of-string-p string index)}
			       !(decf index) ; We just want to peek
			       !(princ #\1 number-stream)]}))
		  (whitespace ()
		    (matchit [@(space-char c)
			       $[@(space-char c)]]))
		  (subcompound ()
		    (matchit
		     [#\(
		      !(progn (push component-list component-list-stack) t)
		      !(progn (setf component-list nil) t)
		      !(main)
		      #\)
		      !(number)
		      !(force-string-accumulation)
		      !(let ((restored-component-list
			      (pop component-list-stack)))
			 (push (list (nreverse component-list)
				     (parse-integer number))
			       restored-component-list)
			 (setf component-list restored-component-list))]))
		  (main ()
		    (matchit
		     [[{[!(element-symbol)
			 !(number)
			 !(force-string-accumulation)
			 !(push (strings->element-list element-symbol number)
				component-list)]
			!(subcompound)}
		      $[!(whitespace)
			{!(element-symbol) !(subcompound)}
			!(number)
			!(force-string-accumulation)
			!(push (strings->element-list element-symbol number)
			       component-list)]]])))
      (matchit
       !(main))))
  (nreverse component-list))

;(parse-compound "C6 H12 O6")
;(parse-compound "(C O2)4 H2")