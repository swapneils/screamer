;;; -*- Mode: LISP; Package: (SCREAMER :USE CL :COLON-MODE :EXTERNAL); Base: 10; Syntax: Ansi-common-lisp -*-

;;; LaHaShem HaAretz U'Mloah

;;; Screamer
;;; A portable efficient implementation of nondeterministic Common Lisp
;;; Based on original version 3.20 by:
;;;
;;;   Jeffrey Mark Siskind (Department of Computer Science, University of Toronto)
;;;   David Allen McAllester (MIT Artificial Intelligence Laboratory)
;;;
;;; Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;; Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;; Copyright 1993 University of Toronto. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright and authorship notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Important notice: In this version of Screamer, if Screamer is already
;;; loaded and you wish to recompile the entire file, the recompilation will
;;; proceed much faster if you first do:
;;; (CLRHASH SCREAMER::*FUNCTION-RECORD-TABLE*)

(in-package :screamer)

(declaim (declaration magic))

(defmacro define-screamer-package (defined-package-name &body options)
  "Convenience wrapper around DEFPACKAGE. Passes its argument directly
to DEFPACKAGE, and automatically injects two additional options:

    \(:shadowing-import-from :screamer :defun :multiple-value-bind :y-or-n-p)
    \(:use :cl :screamer)"
  `(defpackage ,defined-package-name
     ,@options
     (:shadowing-import-from :screamer :defun :multiple-value-bind :y-or-n-p)
     (:use :cl :screamer)))

(define-screamer-package :screamer-user)

;;; Needed because Allegro has some bogosity whereby
;;; (MACRO-FUNCTION <m> <e>) returns NIL during compile time
;;; when <m> is a macro being defined for the first time in
;;; the file being compiled, and LW has similar issues at the
;;; very least in its cross-reference information check.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defmacro-compile-time (function-name lambda-list &body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmacro ,function-name ,lambda-list ,@body))))

(defmacro-compile-time defstruct-compile-time (options &body items)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defstruct ,options ,@items)))

(defmacro-compile-time defvar-compile-time (name &optional initial-value documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defvar ,name ,initial-value ,@(when documentation (list documentation)))))
(defmacro-compile-time defparameter-compile-time (name &optional initial-value documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defparameter ,name ,initial-value ,@(when documentation (list documentation)))))
(defmacro-compile-time defconst-compile-time (name &optional initial-value documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (serapeum:defconst ,name ,initial-value ,@(when documentation (list documentation)))))

(defmacro-compile-time defun-compile-time (function-name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl:defun ,function-name ,lambda-list ,@body)
     (eval-when (:compile-toplevel) (compile ',function-name))))

(defmacro-compile-time define-condition-compile-time (name (&rest parent-types) (&rest slot-specs) &body options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-condition ,name ,parent-types ,slot-specs ,@options)))

(defparameter-compile-time *screamer-version* (asdf:component-version (asdf:find-system :screamer))
  "The version of Screamer which is loaded.")

(defparameter-compile-time *screamer-max-failures* nil
  "The maximum number of failures Screamer will accept in a nondeterministic context
before giving up.

It is recommended that this remain globally NIL. To configure this behavior, set it
via a `let' form around a particular invocation of FOR-EFFECTS or its derivative forms
(e.g. ONE-VALUE, ALL-VALUES).

If this is NIL (the default) Screamer will keep searching forever.
If this is a positive integer, once that number of failures is reached any call to `fail'
will exit the closest FOR-EFFECTS form and reset the failure counter.

If there are multiple lexical definitions of `*screamer-max-failures*' within the same
`for-effects' form, the count of failures will be maintained at the level of the `for-effects'
form, but will not be incremented in dynamic contexts where `*screamer-max-failures*' is NIL.")
(defvar-compile-time *screamer-failures* 0
  "Tracks the number of failures for *screamer-max-failures*. Defaults to 0.
This should only be modified by the `fail' function and `for-effects' macro.")
(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (type (integer 0) *screamer-failures*)
              (type (or null (integer 0)) *screamer-max-failures*)))

(defvar-compile-time *dynamic-extent?* t
  "DEPRECATED: Currently has no effect.

Previously controlled the use of dynamic-extent declarations
in `possibly-beta-reduce-funcall'.

Prior description:
Set to T to enable the dynamic extent optimization, NIL to
disable it. Default is platform dependent.")

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (boolean *iscream?*)))
(defvar-compile-time *iscream?* nil
  "T if Screamer is running under ILisp/GNUEmacs with iscream.el loaded.")

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (type (or null hash-table) *nondeterministic-context*)))
(defvar-compile-time *nondeterministic-context* nil
  "Context for nondeterministic execution.
This must be globally NIL.

In nondeterministic forms, this is set to an equal hash-table,
which can be used to store other information about the
nondeterministic execution.
(see for instance `collect-trail')

Nested nondeterministic forms share the same instance of this
hash-table.")

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (boolean *screamer?*)))
(defvar-compile-time *screamer?* nil
  "This must be NIL except when defining internal Screamer functions.")

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (boolean *compiling-nondeterministic-context?*)))
(defvar-compile-time *compiling-nondeterministic-context?* nil
  "This must be globally NIL.")

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (boolean *local?*)))
(defvar-compile-time *local?* nil "This must be globally NIL.")

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (list *block-tags*)))
(defvar-compile-time *block-tags* '() "This must be globally NIL.")

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (list *tagbody-tags*)))
(defvar-compile-time *tagbody-tags* '() "This must be globally NIL.")

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (vector *trail*)))
(defvar-compile-time *trail* (make-array 4096 :adjustable t :fill-pointer 0) "The trail.")

(defmacro-compile-time with-trail (size-form &rest body)
  "Evaluates the BODY forms with *trail* set to a new array of the specified size. (*trail* is part of Screamer's backtracking mechanism.)
SIZE-FORM is a positive integer or a form which evaluates to a positive integer, used as the size of the *trail* array."
  (when (numberp size-form) (assert (and (integerp size-form) (>= size-form 0))))
  `(let ((screamer::*trail* (make-array
                             (or ,size-form 2048)
                             :adjustable t
                             :fill-pointer 0)))
     ,@body))

(defvar-compile-time *screamer-results* nil
  "A global variable storing the results of the nearest enclosing `-VALUES' or `-VALUES-PROB' form.
Can be used for finer-grained control of nondeterminism.")

(defparameter-compile-time *possibility-consolidator* nil
  "EXPERIMENTAL
If non-nil, must be a function which compares 2 values, used for combining
possibilities generated in ALL-VALUES and ALL-VALUES-PROB.

This is meant as a simple convenience config, but doesn't play well with some
other experimental features like `call/cc', so treating as experimental
for now.")

(defvar-compile-time *numeric-bounds-collapse-threshold* 0.0000000000001
  "The threshold of closeness to consider 2 numbers equivalent.
Use this to deal with floating-point errors, if necessary.

This value must be a floating point number between 0 and 1.")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type (single-float 0.0 1.0) *numeric-bounds-collapse-threshold*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline roughly-=)))
(defun-compile-time roughly-= (a b)
  ;; "Tests approximate numeric equality using `*numeric-bounds-collapse-threshold*'"
  (declare (optimize (speed 3) (debug 0)))
  (or (= a b)
      ;; For floats, also allow them to be "close enough"
      (and (floatp a) (floatp b)
           (<= (abs (- a b))
               *numeric-bounds-collapse-threshold*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline roughly-<=)))
(defun-compile-time roughly-<= (a b)
  ;; "Tests approximate numeric equality using `*numeric-bounds-collapse-threshold*'"
  (declare (optimize (speed 3) (debug 0)))
  (or (<= a b) (roughly-= a b)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline roughly->=)))
(defun-compile-time roughly->= (a b)
  ;; "Tests approximate numeric equality using `*numeric-bounds-collapse-threshold*'"
  (declare (optimize (speed 3) (debug 0)))
  (or (>= a b) (roughly-= a b)))

(defvar-compile-time *cons-cache* (cons nil nil)
  "A cache of conses, to hopefully reduce memory usage")
(defvar-compile-time *cons-cache-len* 0
  "DO NOT MODIFY THIS!!!")
(declaim (type (and fixnum (integer 1)) +cons-cache-max-len+))
(defconst-compile-time +cons-cache-max-len+ 2048
  "Note: Must be between 1 and (1- `most-positive-fixnum')")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline cached-cons)))
(defun-compile-time cached-cons (a b)
  (declare
   (list *cons-cache*)
   (type (and fixnum (integer 0))
         *cons-cache-len*)
   (optimize (speed 3)
             (space 3)
             (debug 0)))
  (if (cdr *cons-cache*)
      (let ((c (cdr *cons-cache*)))
        (setf (cdr *cons-cache*) (cdr c)
              (car c) a (cdr c) b)
        (decf *cons-cache-len*)
        c)
      (cons a b)))

(defun-compile-time release-cons (c)
  (declare
   (list *cons-cache*)
   (type (and fixnum (integer 0))
         *cons-cache-len*)
   (optimize (speed 3)
             (space 3)
             (debug 0)))
  (when (and (consp c)
             ;; Cache isn't already full to capacity.
             (< *cons-cache-len* +cons-cache-max-len+))
    ;; Insert cons as the second element of the cache
    (rotatef (cdr c) (cdr *cons-cache*) c)
    ;; NOTE: Remove contained objects for garbage collection
    (setf (cadr *cons-cache*) nil)
    (incf *cons-cache-len*)))

(defun-compile-time possibly-load-time-value-form (form &optional environment)
  (if (constantp form environment)
    `(load-time-value ,form)
    form))

(defmacro-compile-time cached-list-internal (v &rest vals &environment env)
  `(cached-cons
    ,(possibly-load-time-value-form v env)
    ,(when vals `(cached-list-internal ,@vals))))
(defmacro-compile-time cached-list (v &rest vals)
  `(the list (cached-list-internal ,v ,@vals)))

(defmacro-compile-time cached-list*-internal (v &optional v2 &rest vals &environment env)
  `(cached-cons
    ,(possibly-load-time-value-form v env)
    ,(if vals
         `(cached-list* ,v2 ,@vals)
         (possibly-load-time-value-form v2 env))))
(defmacro-compile-time cached-list* (v &rest vals)
  `(the list (cached-list*-internal ,v ,@vals)))

(defun-compile-time release-list (l)
  (declare (list l)
           (optimize (speed 3)
                     (space 3)
                     (debug 0)))
  (iter:iter
    (iter:for x initially l then y)
    (iter:for y = (cdr x))
    (iter:while y)
    (release-cons x)))

(defmacro-compile-time cached-push (v place)
  `(setf ,place (cached-cons ,v ,place)))

(defun-compile-time cached-mapcar (f s)
  "Mapcar on one sequence at a time, but uses
`*cons-cache*' to reduce consing.

Note that performance optimizations can rarely
be generalized; use your own judgement and
experimentation to determine the value of this
in comparison to `cl:mapcar'"
  (s:nest
   ;; Remove the `nil' added to the front
   (rest)
   ;; Get the head of the collected list
   ;; Note that the first item was tracking
   ;; the tail of the collection so we could
   ;; keep adding to the end.
   (second)
   (reduce (lambda (a b)
             (prog2 (setf (cdr (first a))
                          (cached-list (funcall f b)))
                 (cached-cons (rest (car a)) (cdr a))
               (release-cons a)))
           s
           :initial-value
           (let ((temp (cached-cons s nil)))
             (cached-cons temp temp)))))


(defun-compile-time notf (f)
  (lambda (&rest xs)
    (not (apply f xs))))
(defun-compile-time andf (&rest fs)
  (lambda (&rest xs)
    (every (rcurry #'apply xs) fs)))
(defun-compile-time orf (&rest fs)
  (lambda (&rest xs)
    (some (rcurry #'apply xs) fs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (hash-table *function-record-table*)))
(defvar-compile-time *function-record-table* (make-hash-table :test #'equal)
  "The function record table.")

(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (list *ordered-lambda-list-keywords*)))
(defvar-compile-time *ordered-lambda-list-keywords*
    '(&optional &rest &key &allow-other-keys &aux)
  "The allowed lambda list keywords in order.")

(defmacro-compile-time choice-point-internal (form)
  `(catch '%fail
     (let* ((toplevel (typep *nondeterministic-context* '(not null)))
            (*nondeterministic-context* (or *nondeterministic-context* (s:dict))))
       (declare (ignorable toplevel))
       (unwind-protect ,form
         (unwind-trail-to trail-pointer)))))

(defmacro-compile-time choice-point-external (&rest forms)
  ;; NOTE: Is it really better to use VECTOR-PUSH-EXTEND than CONS for the
  ;;       trail?
  `(let ((trail-pointer (fill-pointer *trail*))) ,@forms))

(defmacro-compile-time choice-point (form)
  `(choice-point-external (choice-point-internal ,form)))

(defstruct-compile-time function-record
  function-name
  (lambda-list nil)
  (body nil)
  (callees nil)
  (deterministic? t)
  (old-deterministic? nil)
  (screamer? *screamer?*))

(defstruct-compile-time (nondeterministic-function
                         (:print-function print-nondeterministic-function)
                         (:predicate nondeterministic-function?-internal))
  function)

(define-condition-compile-time screamer-error (error)
    ((message :initarg :message :initform nil)
     (args :initarg :args :initform nil))
  (:documentation "Class for errors thrown by Screamer.")
  (:report (lambda (condition stream)
             (format stream
                     "Encountered Screamer error:~2%~A"
                     (apply #'format nil
                            (slot-value condition 'message)
                            (slot-value condition 'args))))))

(defun-compile-time screamer-error (header &rest args)
  (error 'screamer-error
         :message (concatenate
                   'string
                   header
                   "~2%There are eight types of nondeterministic contexts:

  1. the body of a function defined with SCREAMER::DEFUN
  2. the body of a FOR-EFFECTS macro invocation
  3. the body of an ALL-VALUES or ALL-VALUES-PROB macro invocation
  4. the body of an N-VALUES or N-VALUES-PROB macro invocation
  5. the first argument of a ONE-VALUE macro invocation
  6. the body of a PRINT-VALUES macro invocation
  7. the second argument of an ITH-VALUE or KTH-VALUE macro invocation
  8. the body of a POSSIBLY? macro invocation
  9. the body of a NECESSARILY? macro invocation.
 10. the body of a UNIQUELY? macro invocation.

Note that the default forms of &OPTIONAL and &KEY arguments and the
initialization forms of &AUX variables are always deterministic
contexts even though they may appear inside a SCREAMER::DEFUN.")
         :args args))

(defun-compile-time get-function-record (function-name)
  (or (gethash function-name *function-record-table*)
      (setf (gethash function-name *function-record-table*)
            (make-function-record :function-name function-name))))

(defun-compile-time peal-off-documentation-string-and-declarations
    (body &optional documentation-string?)
  ;; NOTE: This will need to be done as well for LOCALLY and MACROLET when we
  ;;       eventually implement them.
  ;; TODO: This requires that the documentation string preceed all
  ;;       declarations which needs to be fixed.
  (let (documentation-string declarations)
    (when (and documentation-string?
               (not (null body))
               (not (null (rest body)))
               (stringp (first body)))
      (setf documentation-string (first body))
      (setf body (rest body)))
    (loop (unless (and (not (null body))
                       (consp (first body))
                       (eq (first (first body)) 'declare))
            (return))
          (cached-push (first body) declarations)
          (pop body))
    (values body (reverse declarations) documentation-string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline self-evaluating?)))
(defun-compile-time self-evaluating? (thing)
  (declare (optimize (speed 3) (space 3)))
  (and (not (consp thing))
       (or (not (symbolp thing))
           (null thing)
           (eq thing t)
           (eq (symbol-package thing) (symbol-package :x)))))

(defun-compile-time valid-macro? (thing &optional env)
  (declare (optimize (speed 3) (space 3) (debug 0)))
  (and (consp thing)
       (symbolp (first thing))
       (macro-function (first thing) env)
       ;; NOTE: Relying on user and implementation
       ;; to detect this failure-case
       ;; ;; Doesn't expand to itself
       ;; (not (equal (macroexpand thing env) thing))
       ))
(defun-compile-time external-macro? (thing &optional env)
  (and (valid-macro? thing env)
       (not (member (symbol-package (first thing))
                    '(:screamer)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline quotify)))
(defun-compile-time quotify (thing)
  (if (self-evaluating? thing) thing `',thing))

(defun-compile-time lambda-expression? (form)
  (and (consp form)
       (eq (first form) 'lambda)
       (or (and (null (rest (last form)))
                (>= (length form) 2)
                (listp (second form)))
           (error "Invalid syntax for LAMBDA expression: ~S" form))))

(defun-compile-time valid-function-name? (function-name)
  (or (and (symbolp function-name) (not (null function-name)))
      (and (consp function-name)
           (eq (first function-name) 'setf)
           (null (rest (last function-name)))
           (= (length function-name) 2)
           (symbolp (second function-name))
           (not (null (second function-name))))))

(defun-compile-time check-function-name (function-name)
  (unless (valid-function-name? function-name)
    (error "Invalid function name: ~S" function-name)))

(defun-compile-time every-other (list)
  (cond ((null list) list)
        ((null (rest list)) list)
        (t (cached-cons (first list) (every-other (rest (rest list)))))))

(defun-compile-time check-lambda-list-internal (lambda-list &optional mode)
  (cond
    ((null lambda-list))
    ((member (first lambda-list) *ordered-lambda-list-keywords* :test #'eq)
     (check-lambda-list-internal (rest lambda-list) (first lambda-list)))
    (t (let ((parameter (first lambda-list)))
         (ecase mode
           ((nil)
            (unless (symbolp parameter)
              (error "Invalid parameter: ~S" parameter)))
           (&optional
            (unless (or (symbolp parameter)
                        (and (consp parameter)
                             (null (rest (last parameter)))
                             (or (= (length parameter) 1)
                                 (= (length parameter) 2)
                                 (and (= (length parameter) 3)
                                      (symbolp (third parameter))))
                             (symbolp (first parameter))))
              (error "Invalid &OPTIONAL parameter: ~S" parameter)))
           (&rest
            (unless (symbolp parameter)
              (error "Invalid &REST parameter: ~S" parameter)))
           (&key
            (unless (or (symbolp parameter)
                        (and (consp parameter)
                             (null (rest (last parameter)))
                             (or (= (length parameter) 1)
                                 (= (length parameter) 2)
                                 (and (= (length parameter) 3)
                                      (symbolp (third parameter))))
                             (or (symbolp (first parameter))
                                 (and (consp (first parameter))
                                      (null (rest (last (first parameter))))
                                      (= (length (first parameter)) 2)
                                      (symbolp (first (first parameter)))
                                      (symbolp (second (first parameter)))))))
              (error "Invalid &KEY parameter: ~S" parameter)))
           (&aux
            (unless (or (symbolp parameter)
                        (and (consp parameter)
                             (null (rest (last parameter)))
                             (or (= (length parameter) 1)
                                 (= (length parameter) 2))
                             (symbolp (first parameter))))
              (error "Invalid &AUX parameter: ~S" parameter)))))
       (check-lambda-list-internal (rest lambda-list) mode))))

(defun-compile-time check-lambda-list (lambda-list)
  (unless (null (rest (last lambda-list)))
    (error "Improper lambda-list: ~S" lambda-list))
  (let ((rest (member '&rest lambda-list :test #'eq)))
    (if rest
        (let ((rest (rest rest)))
          (unless (not (member '&rest rest :test #'eq))
            (error "&REST cannot appear more than once: ~S" lambda-list))
          (unless (and (not (null rest))
                       (not (member (first rest) lambda-list-keywords :test #'eq))
                       (or (null (rest rest))
                           (member (first (rest rest)) lambda-list-keywords
                                   :test #'eq)))
            (error "&REST must be followed by exactly one variable: ~S"
                   lambda-list)))))
  (let ((allow-other-keys (member '&allow-other-keys lambda-list :test #'eq)))
    (if allow-other-keys
        (unless (or (null (rest allow-other-keys))
                    (member (first (rest allow-other-keys)) lambda-list-keywords
                            :test #'eq))
          (error "&ALLOW-OTHER-KEYS must not be followed by a parameter: ~S"
                 lambda-list))))
  (let ((keywords
          (remove-if-not #'(lambda (argument)
                             (member argument lambda-list-keywords :test #'eq))
                         lambda-list)))
    (unless (every #'(lambda (keyword)
                       (member keyword *ordered-lambda-list-keywords* :test #'eq))
                   keywords)
      (error "Invalid lambda list keyword: ~S" lambda-list))
    (unless (every #'(lambda (x y)
                       (member y (member x *ordered-lambda-list-keywords*
                                         :test #'eq)
                               :test #'eq))
                   keywords
                   (rest keywords))
      (error "Invalid order for lambda list keywords: ~S" lambda-list)))
  (check-lambda-list-internal lambda-list))

(defun-compile-time walk-lambda-list-reducing
    (map-function reduce-function screamer? partial? nested? lambda-list
                  environment &optional mode)
  (cond
    ((null lambda-list) (funcall reduce-function))
    ((member (first lambda-list) *ordered-lambda-list-keywords* :test #'eq)
     (walk-lambda-list-reducing map-function
                                reduce-function
                                screamer?
                                partial?
                                nested?
                                (rest lambda-list)
                                environment
                                (first lambda-list)))
    (t (ecase mode
         ((nil &rest &allow-other-keys &aux)
          (walk-lambda-list-reducing map-function
                                     reduce-function
                                     screamer?
                                     partial?
                                     nested?
                                     (rest lambda-list)
                                     environment
                                     mode))
         ((&optional &key)
          (if (and (consp (first lambda-list))
                   (consp (rest (first lambda-list))))
              (funcall
               reduce-function
               (walk map-function reduce-function screamer? partial? nested?
                     (second (first lambda-list)) environment)
               (walk-lambda-list-reducing map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          (rest lambda-list)
                                          environment
                                          mode))
              (walk-lambda-list-reducing map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         (rest lambda-list)
                                         environment
                                         mode)))))))

(defun-compile-time walk-lambda-list
    (map-function reduce-function screamer? partial? nested? lambda-list
                  environment)
  (check-lambda-list lambda-list)
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function lambda-list 'lambda-list)
       (walk-lambda-list-reducing map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  lambda-list
                                  environment))
      (funcall map-function lambda-list 'lambda-list)))

;;; TODO: Fix this to get nondeterminism working properly with return-from calls
;;; NOTE: The above with TAGBODY fixes could also allow macroexpanding loops
;;; and then making them nondeterministic, rather than having to put looping
;;; constructs in top-level defuns
;;; NOTE: See the `dotimes' test case for a failing test involving tags
(defun-compile-time walk-block
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper BLOCK: ~S" form))
  (unless (>= (length form) 2)
    (error "BLOCK must have at least one argument, a NAME: ~S" form))
  (unless (symbolp (second form)) (error "NAME must be a symbol: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'block)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest (rest form)))))
      (funcall map-function form 'block)))

(defun-compile-time walk-catch
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper PROGN: ~S" form))
  (unless (>= (length form) 2)
    (error "CATCH must have at least one argument, a TAG: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'catch)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'catch)))

(defun-compile-time walk-eval-when
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper EVAL-WHEN: ~S" form))
  (unless (>= (length form) 2)
    (error "EVAL-WHEN must have at least one argument: ~S" form))
  (unless (listp (second form))
    (error "First argument of EVAL-WHEN must be a list: ~S" form))
  (unless (null (rest (last (second form))))
    (error "Improper list of SITUATIONS: ~S" form))
  (unless (every #'(lambda (situation)
                     (member situation '(:compile-toplevel
                                         :load-toplevel
                                         :execute
                                         compile
                                         load
                                         evel)
                             :test #'eq))
                 (second form))
    (error "Invalid SITUATION: ~S" form))
  (if (member :execute (second form) :test #'eq)
      (walk-progn map-function
                  reduce-function
                  screamer?
                  partial?
                  nested?
                  `(progn ,@(rest (rest form)))
                  environment)
      (funcall map-function nil 'quote)))

;;; TODO: Set up walkers for labels and flet forms
;;; to be converted into local functions on the trail,
;;; NOTE: The above would allow implementing recursion
;;; as local functions, rather than needing SCREAMER::DEFUN
(defun-compile-time walk-flet/labels
    (map-function reduce-function screamer? partial? nested? form environment
                  form-type)
  (unless (null (rest (last form))) (error "Improper ~S: ~S" form-type form))
  (unless (>= (length form) 2)
    (error "~S must have BINDINGS: ~S" form-type form))
  (unless (and (listp (second form))
               (null (rest (last (second form))))
               (every #'(lambda (binding)
                          (and (consp binding)
                               (null (rest (last binding)))
                               (>= (length binding) 2)
                               (valid-function-name? (first binding))
                               (listp (second binding))))
                      (second form)))
    (error "Invalid BINDINGS for ~S: ~S" form-type form))
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function form form-type)
       (if nested?
           (funcall
            reduce-function
            (reduce
             reduce-function
             (mapcar
              #'(lambda (binding)
                  (funcall reduce-function
                           (walk-lambda-list map-function
                                             reduce-function
                                             screamer?
                                             partial?
                                             nested?
                                             (second binding)
                                             environment)
                           (mapcar
                            #'(lambda (subform)
                                (walk map-function
                                      reduce-function
                                      screamer?
                                      partial?
                                      nested?
                                      subform
                                      environment))
                            (peal-off-documentation-string-and-declarations
                             (rest (rest binding)) t))))
              (second form)))
            (reduce reduce-function
                    (mapcar #'(lambda (subform)
                                (walk map-function
                                      reduce-function
                                      screamer?
                                      partial?
                                      nested?
                                      subform
                                      environment))
                            (rest (rest form)))))
           (reduce reduce-function
                   (mapcar #'(lambda (subform)
                               (walk map-function
                                     reduce-function
                                     screamer?
                                     partial?
                                     nested?
                                     subform
                                     environment))
                           (rest (rest form))))))
      (funcall map-function form form-type)))

(defun-compile-time walk-function
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper FUNCTION: ~S" form))
  (unless (= (length form) 2)
    (error "FUNCTION must have one argument: ~S" form))
  (cond ((lambda-expression? (second form))
         (if (and reduce-function nested?)
             (funcall
              reduce-function
              (funcall map-function form 'function-lambda)
              (funcall
               reduce-function
               (walk-lambda-list map-function
                                 reduce-function
                                 screamer?
                                 partial?
                                 nested?
                                 (second (second form))
                                 environment)
               (reduce
                reduce-function
                (mapcar #'(lambda (subform)
                            (walk map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  subform
                                  environment))
                        (peal-off-documentation-string-and-declarations
                         (rest (rest (second form))) t)))))
             (funcall map-function form 'function-lambda)))
        ((valid-function-name? (second form))
         (cond
           ((symbolp (second form))
            (if (or (special-operator-p (second form))
                    (macro-function (second form) environment))
                (error "You can't reference the FUNCTION of a special form or~%~
                      macro: ~S"
                       form)
                (funcall map-function form 'function-symbol)))
           (t (funcall map-function form 'function-setf))))
        (t (error "Invalid argument to FUNCTION: ~S" form))))

(defun-compile-time walk-go (map-function form)
  (unless (null (rest (last form))) (error "Improper GO: ~S" form))
  (unless (= (length form) 2) (error "GO must have one argument: ~S" form))
  (unless (or (symbolp (second form)) (integerp (second form)))
    (error "TAG of GO must be a symbol or integer: ~S" form))
  (funcall map-function form 'go))

(defun-compile-time walk-if
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper IF: ~S" form))
  (unless (or (= (length form) 3) (= (length form) 4))
    (error "IF must have two or three arguments: ~S" form))
  (if reduce-function
      (if (= (length form) 4)
          (funcall reduce-function
                   (funcall map-function form 'if)
                   (funcall reduce-function
                            (walk map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  (second form)
                                  environment)
                            (funcall reduce-function
                                     (walk map-function
                                           reduce-function
                                           screamer?
                                           partial?
                                           nested?
                                           (third form)
                                           environment)
                                     (walk map-function
                                           reduce-function
                                           screamer?
                                           partial?
                                           nested?
                                           (fourth form)
                                           environment))))
          (funcall reduce-function
                   (funcall map-function form 'if)
                   (funcall reduce-function
                            (walk map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  (second form)
                                  environment)
                            (walk map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  (third form)
                                  environment))))
      (funcall map-function form 'if)))

(defun-compile-time walk-let/let*
    (map-function reduce-function screamer? partial? nested? form environment
                  form-type)
  (unless (null (rest (last form))) (error "Improper ~S: ~S" form-type form))
  (unless (>= (length form) 2)
    (error "~S must have BINDINGS: ~S" form-type form))
  (unless (and (listp (second form))
               (null (rest (last (second form))))
               (every #'(lambda (binding)
                          (or (symbolp binding)
                              (and (consp binding)
                                   (null (rest (last binding)))
                                   (or (= (length binding) 1)
                                       (= (length binding) 2))
                                   (symbolp (first binding)))))
                      (second form)))
    (error "Invalid BINDINGS for ~S: ~S" form-type form))
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function form form-type)
       (funcall reduce-function
                (reduce reduce-function
                        (mapcar #'(lambda (binding)
                                    (walk map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          (second binding)
                                          environment))
                                (remove-if-not
                                 #'(lambda (binding)
                                     (and (consp binding)
                                          (= (length binding) 2)))
                                 (second form))))
                (reduce reduce-function
                        (mapcar #'(lambda (subform)
                                    (walk map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          subform
                                          environment))
                                (peal-off-documentation-string-and-declarations
                                 (rest (rest form)))))))
      (funcall map-function form form-type)))

(defun-compile-time walk-multiple-value-call
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form)))
    (error "Improper MULTIPLE-VALUE-CALL: ~S" form))
  (unless (>= (length form) 2)
    (error "MULTIPLE-VALUE-CALL must have at least one argument, a FUNCTION: ~S"
           form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'multiple-value-call)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'multiple-value-call)))

(defun-compile-time walk-multiple-value-prog1
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form)))
    (error "Improper MULTIPLE-VALUE-PROG1: ~S" form))
  (unless (>= (length form) 2)
    (error "MULTIPLE-VALUE-PROG1 must have at least one argument, a FORM: ~S"
           form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'multiple-value-prog1)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'multiple-value-prog1)))

(defun-compile-time walk-progn
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper PROGN: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'progn)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'progn)))

(defun-compile-time walk-progv
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper PROGV: ~S" form))
  (unless (>= (length form) 3)
    (error "PROGV must have at least two arguments: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'progv)
               (funcall reduce-function
                        (funcall reduce-function
                                 (walk map-function
                                       reduce-function
                                       screamer?
                                       partial?
                                       nested?
                                       (second form)
                                       environment)
                                 (walk map-function
                                       reduce-function
                                       screamer?
                                       partial?
                                       nested?
                                       (third form)
                                       environment))
                        (reduce reduce-function
                                (mapcar #'(lambda (subform)
                                            (walk map-function
                                                  reduce-function
                                                  screamer?
                                                  partial?
                                                  nested?
                                                  subform
                                                  environment))
                                        (rest (rest (rest form)))))))
      (funcall map-function form 'progv)))

(defun-compile-time walk-quote (map-function form)
  (unless (null (rest (last form))) (error "Improper QUOTE: ~S" form))
  (unless (= (length form) 2)
    (error "QUOTE must have one argument: ~S" form))
  (funcall map-function (second form) 'quote))

(defun-compile-time walk-return-from
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper RETURN-FROM: ~S" form))
  (unless (or (= (length form) 2) (= (length form) 3))
    (error "RETURN-FROM must have one or two arguments,~%~
          a NAME and an optional RESULT: ~S" form))
  (unless (symbolp (second form)) (error "NAME must be a symbol: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'return-from)
               (walk map-function
                     reduce-function
                     screamer?
                     partial?
                     nested?
                     (if (= (length form) 3) (third form) nil)
                     environment))
      (funcall map-function form 'return-from)))

(defun-compile-time walk-setq
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper SETQ: ~S" form))
  (unless (every #'symbolp (every-other (rest form)))
    (error "Invalid destination for SETQ: ~S" form))
  (unless (evenp (length (rest form)))
    (error "Odd number of arguments to SETQ: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'setq)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (every-other (rest (rest form))))))
      (funcall map-function form 'setq)))

;;; TODO: Get this to work properly with tag calls
(defun-compile-time walk-tagbody
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper TAGBODY: ~S" form))
  (unless (every #'(lambda (subform)
                     (or (symbolp subform) (integerp subform) (listp subform)))
                 (rest form))
    (error "A subforms of a TAGBODY must be symbols, integers or lists: ~S"
           form))
  (let ((tags (remove-if #'consp (rest form))))
    (unless (= (length tags) (length (remove-duplicates tags)))
      (error "TAGBODY has duplicate TAGs: ~S" form)))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'tagbody)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (remove-if-not #'consp (rest form)))))
      (funcall map-function form 'tagbody)))

(defun-compile-time walk-the
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper THE: ~S" form))
  (unless (= (length form) 3) (error "THE must have two arguments: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (walk map-function
                     reduce-function
                     screamer?
                     partial?
                     nested?
                     (third form)
                     environment)
               (funcall map-function form 'the))
      (funcall map-function form 'the)))

(defun-compile-time walk-throw
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper THROW: ~S" form))
  (unless (= (length form) 3)
    (error "THROW must have two arguments, a TAG and a RESULT: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'throw)
               (funcall reduce-function
                        (walk map-function
                              reduce-function
                              screamer?
                              partial?
                              nested?
                              (second form)
                              environment)
                        (walk map-function
                              reduce-function
                              screamer?
                              partial?
                              nested?
                              (third form)
                              environment)))
      (funcall map-function form 'throw)))

(defun-compile-time walk-unwind-protect
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper UNWIND-PROTECT: ~S" form))
  (unless (>= (length form) 2)
    (error "UNWIND-PROTECT must have at least one argument, a PROTECTED-FORM: ~S"
           form))
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function form 'unwind-protect)
       (funcall reduce-function
                (walk map-function
                      reduce-function
                      screamer?
                      partial?
                      nested?
                      (second form)
                      environment)
                (reduce reduce-function
                        (mapcar #'(lambda (subform)
                                    (walk map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          subform
                                          environment))
                                (rest (rest form))))))
      (funcall map-function form 'unwind-protect)))

(defun-compile-time walk-handler-bind
    (map-function reduce-function screamer? partial? nested? form environment
                  &aux (form-type 'handler-bind))
  (unless (null (rest (last form))) (error "Improper ~S: ~S" form-type form))
  (unless (>= (length form) 2)
    (error "~S must have BINDINGS: ~S" form-type form))
  (unless (and (listp (second form))
               (null (rest (last (second form))))
               (every #'(lambda (binding)
                          (and (consp binding)
                                   (null (rest (last binding)))
                                   (or (= (length binding) 1)
                                       (= (length binding) 2))
                                   (symbolp (first binding))))
                      (second form)))
    (error "Invalid BINDINGS for ~S: ~S" form-type form))
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function form form-type)
       (funcall reduce-function
                (reduce reduce-function
                        (mapcar #'(lambda (binding)
                                    (walk map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          (second binding)
                                          environment))
                                (second form)))
                (reduce reduce-function
                        (mapcar #'(lambda (subform)
                                    (walk map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          subform
                                          environment))
                                (rest (rest form))))))
      (funcall map-function form form-type)))

(defun-compile-time walk-handler/restart-case
    (map-function reduce-function screamer? partial? nested? form environment
                  form-type)
  (unless (null (rest (last form))) (error "Improper ~S: ~S" form-type form))
  (let ((bindings (rest (rest form))))
    (unless (and (listp bindings)
                 (null (rest (last bindings)))
                 (every #'(lambda (binding)
                            (and (consp binding)
                                 (null (rest (last binding)))
                                 ;; type
                                 (symbolp (first binding))
                                 ;; handler arglist
                                 (listp (second binding))))
                        bindings))
      (error "Invalid BINDINGS for ~S: ~S" form-type form)))
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function form 'unwind-protect)
       (funcall reduce-function
                (walk map-function
                      reduce-function
                      screamer?
                      partial?
                      nested?
                      (second form)
                      environment)
                (reduce reduce-function
                        (mapcar #'(lambda (binding)
                                    (walk map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          `(lambda (second binding)
                                             ,@(rest (rest binding)))
                                          environment))
                                (rest (rest form))))))
      (funcall map-function form 'unwind-protect)))

(defun-compile-time walk-ignore-errors
    (map-function reduce-function screamer? partial? nested? form environment
                  &aux (form-type 'ignore-errors))
  (unless (null (rest (last form))) (error "Improper ~S: ~S" form form-type))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'ignore-errors)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'ignore-errors)))


(defun-compile-time walk-for-effects
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper FOR-EFFECTS: ~S" form))
  ;; NOTE: We used to think that we should never walk the body of FOR-EFFECTS
  ;;       as we thought that the walker would get confused on the code
  ;;       generated by FOR-EFFECTS and that FOR-EFFECTS called
  ;;       CPS-CONVERT-PROGN on its body and that CPS-CONVERT-PROGN did the
  ;;       walk for us. But that was wrong since FORM-CALLEES also walks and
  ;;       thus would miss functions called in the body of a FOR-EFFECTS. So now
  ;;       we walk the body of a FOR-EFFECTS without macro-expanding it, but
  ;;       only when NESTED? is true which is essentially only for FORM-CALLEES
  ;;       since DETERMINISTIC? must not walk the body of FOR-EFFECTS or else
  ;;       it will mistakingly report that that a FOR-EFFECTS form is
  ;;       nondeterministic when its body is nondeterministic.
  (if (and reduce-function nested?)
      (funcall reduce-function
               (funcall map-function form 'for-effects)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'for-effects)))

(defun-compile-time walk-setf
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper SETF: ~S" form))
  (unless (evenp (length (rest form)))
    (error "Odd number of arguments to SETF: ~S" form))
  (if *local?*
      (if reduce-function
          (funcall reduce-function
                   (funcall map-function form 'local-setf)
                   (reduce reduce-function
                           (mapcar #'(lambda (subform)
                                       (walk map-function
                                             reduce-function
                                             screamer?
                                             partial?
                                             nested?
                                             subform
                                             environment))
                                   (every-other (rest (rest form))))))
          (funcall map-function form 'local-setf))
      (walk map-function
            reduce-function
            screamer?
            partial?
            nested?
            (let ((*macroexpand-hook* #'funcall))
              (macroexpand-1 form environment))
            environment)))

(defun-compile-time walk-multiple-value-call-nondeterministic
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form)))
    (error "Improper MULTIPLE-VALUE-CALL-NONDETERMINISTIC: ~S" form))
  (unless (>= (length form) 2)
    (error "MULTIPLE-VALUE-CALL-NONDETERMINISTIC must have at least one ~
          argument, a FUNCTION: ~S"
           form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'multiple-value-call-nondeterministic)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'multiple-value-call-nondeterministic)))

(defun-compile-time walk-full (map-function form)
  (unless (null (rest (last form))) (error "Improper FULL: ~S" form))
  (unless (= (length form) 2)
    (error "FULL must have exactly one argument, a FORM: ~S" form))
  (funcall map-function form 'full))

(defun-compile-time walk-macro-call
    (map-function reduce-function screamer? partial? nested? form environment)
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'macro-call)
               (walk map-function
                     reduce-function
                     screamer?
                     partial?
                     nested?
                     (let ((*macroexpand-hook* #'funcall))
                       (macroexpand-1 form environment))
                     environment))
      (walk map-function
            reduce-function
            screamer?
            partial?
            nested?
            (let ((*macroexpand-hook* #'funcall))
              (macroexpand-1 form environment))
            environment)))

(defun-compile-time walk-function-call
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form)))
    (error "Improper function call form: ~S" form))
  (cond
    ((lambda-expression? (first form))
     (if reduce-function
         (funcall
          reduce-function
          (funcall map-function form 'lambda-call)
          (funcall
           reduce-function
           (reduce reduce-function
                   (mapcar #'(lambda (subform)
                               (walk map-function
                                     reduce-function
                                     screamer?
                                     partial?
                                     nested?
                                     subform
                                     environment))
                           (rest form)))
           (funcall
            reduce-function
            (walk-lambda-list map-function
                              reduce-function
                              screamer?
                              partial?
                              nested?
                              (second (first form))
                              environment)
            (reduce reduce-function
                    (mapcar #'(lambda (subform)
                                (walk map-function
                                      reduce-function
                                      screamer?
                                      partial?
                                      nested?
                                      subform
                                      environment))
                            (peal-off-documentation-string-and-declarations
                             (rest (rest (first form))) t))))))
         (funcall map-function form 'lambda-call)))
    ((valid-function-name? (first form))
     (if (symbolp (first form))
         (if reduce-function
             (funcall reduce-function
                      (funcall map-function form 'symbol-call)
                      (reduce reduce-function
                              (mapcar #'(lambda (subform)
                                          (walk map-function
                                                reduce-function
                                                screamer?
                                                partial?
                                                nested?
                                                subform
                                                environment))
                                      (rest form))))
             (funcall map-function form 'symbol-call))
         (if reduce-function
             (funcall reduce-function
                      (funcall map-function form 'setf-call)
                      (reduce reduce-function
                              (mapcar #'(lambda (subform)
                                          (walk map-function
                                                reduce-function
                                                screamer?
                                                partial?
                                                nested?
                                                subform
                                                environment))
                                      (rest form))))
             (funcall map-function form 'setf-call))))
    (t (error "CAR of form ~S is not a valid function" form))))

;;; Possible FORM-TYPEs
;;;  Other:
;;;   LAMBDA-LIST VARIABLE
;;;  Special forms:
;;;   BLOCK CATCH EVAL-WHEN FLET FUNCTION-LAMBDA FUNCTION-SYMBOL FUNCTION-SETF
;;;   GO IF LABELS LET LET* MULTIPLE-VALUE-CALL MULTIPLE-VALUE-PROG1 PROGN
;;;   PROGV QUOTE RETURN-FROM SETQ TAGBODY THE THROW UNWIND-PROTECT
;;;  Symbolics special forms:
;;;   SYS:VARIABLE-LOCATION COMPILER:INVISIBLE-REFERENCES
;;;  Screamer special forms:
;;;   FOR-EFFECTS LOCAL-SETF
;;;  Partial special forms:
;;;   FULL
;;;  Other:
;;;   MACRO-CALL LAMBDA-CALL SYMBOL-CALL SETF-CALL

;;; TODO: Improve testing for macrolet/symbol-macrolet

(defvar-compile-time *screamer-macroexpansions* nil
  "internal alist tracking lexical macros while they are expanded by `walk'")
(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (list *screamer-macroexpansions*)))

(defun-compile-time expand-lexical-environments (form &optional environment)
  "EXPERIMENTAL
Expands macrolet/symbol-macrolet forms and their invocations.
Returns 2 values, a modified form to be processed by WALK and
the updated value of `*screamer-macroexpansions*'.

SHOULD NOT BE INVOKED OUTSIDE OF `walk'!"
  (declare
   (optimize (speed 3) (space 3) (debug 0)))
  (let ((*screamer-macroexpansions* *screamer-macroexpansions*))
    (macrolet ((call-expansion (form env)
                 `(destructuring-bind (f e)
                      (expand-lexical-environments ,form ,env)
                    ;; (print (list 'recursive 'form f 'expansions e))
                    (setf *screamer-macroexpansions* e)
                    f)))
      (list
       (ematch form
         ;; If an empty declare form, get rid of it
         ;; TODO: Do we need this?
         ((list 'declare) nil)
         ;; If a quoted form or declaration, ignore it
         ((list* (or 'quote 'declare) _) form)
         ;; If an empty macrolet or symbol-macrolet, ignore it
         ((list* (or 'macrolet 'symbol-macrolet) nil body)
          `(let nil ,@body))
         ;; If a macrolet, expand and save the defs and expand the body.
         ((list* 'macrolet defs body)
          ;; Iterate through the defs and collect their lexical definitions
          (iter:iter
            ;; Removing duplicate definitions the same way SBCL resolves them,
            ;; to avoid infinite loops
            (iter:for (name args . def-body) in (remove-duplicates defs :key #'first :from-end t))
            (let* (
                   ;; Walk def. This automatically expands it as well,
                   ;; using the current lexical environment to do so.
                   (new-def-let-wrapper
                     (walk (lambda (form form-type) (declare (ignore form-type)) form) nil
                           t nil nil
                           `(let () ,@def-body) environment))
                   (new-def-body (nthcdr 2 new-def-let-wrapper))
                   ;; Create macro function for def
                   ;; FIXME: Uses EVAL!!!
                   (replacer (eval `(lambda ,args ,@new-def-body))))
              ;; Push macro function to tracked lexical environment
              (push `(,name . ,replacer) *screamer-macroexpansions*)))
          ;; NOTE: Old version
          ;; Push the lexical macro definitions onto `*screamer-macroexpansions*'
          ;; (setf *screamer-macroexpansions*
          ;;       (append
          ;;        ;; Iterate through the defs and collect their lexical definitions
          ;;        (iter:iter
          ;;          (iter:for (name args . def-body) in defs)
          ;;          (let* (
          ;;                 ;; Walk def. This automatically expands it as well,
          ;;                 ;; using the current lexical environment to do so.
          ;;                 (new-def-let-wrapper
          ;;                   (walk (lambda (form form-type) (declare (ignore form-type)) form) nil
          ;;                         t nil nil
          ;;                         `(let () ,@def-body) environment))
          ;;                 (new-def-body (nthcdr 2 new-def-let-wrapper))
          ;;                 ;; Create macro function for def
          ;;                 ;; FIXME: Uses EVAL!!!
          ;;                 (replacer (eval `(lambda ,args ,@new-def-body))))
          ;;            ;; Collect macro function to add to tracked lexical environment
          ;;            (iter:collect `(,name . ,replacer))))
          ;;        *screamer-macroexpansions*))
          ;; Expand the body with the new lexical macroexpansions
          `(let nil ,@(mapcar (compose #'first (rcurry #'expand-lexical-environments environment)) body)))
         ;; If a symbol-macrolet, expand and save the defs and expand the body.
         ((list* 'symbol-macrolet defs body)
          ;; Iterate through the defs and collect their lexical definitions
          (iter:iter
            ;; Removing duplicate definitions the same way SBCL resolves them,
            ;; to avoid infinite loops
            (iter:for (name . def-body) in (remove-duplicates defs :key #'first :from-end t))
            (let* (
                   ;; FIXME: Uses EVAL!!!
                   ;; Create macro function for def
                   (replacer (eval `(lambda () ',@def-body))))
              ;; (print (list 'symbol-macro-body `(',@def-body)))
              ;; Push labelled symbol macro function to the tracked lexical environment
              (push `((symbol ,name) . ,replacer) *screamer-macroexpansions*)))
          ;; NOTE: Old version
          ;; Push the lexical symbol-macro definitions onto `*screamer-macroexpansions*'
          ;; (setf *screamer-macroexpansions*
          ;;       (append
          ;;        ;; Iterate through the defs and collect their lexical definitions
          ;;        (iter:iter
          ;;          (iter:for (name . def-body) in defs)
          ;;          (let* (
          ;;                 ;; Expand def based on parent env
          ;;                 ;; (new-def-body (mapcar (compose #'first (rcurry #'expand-lexical-environments environment)) def-body))
          ;;                 ;; FIXME: Currently does nothing!!!
          ;;                 ;; (new-def-body def-body)
          ;;                 ;; Create macro function for def
          ;;                 (replacer (eval `(lambda () ',@def-body))))
          ;;            ;; (print (list 'symbol-macro-body `(',@def-body)))
          ;;            ;; Collect labelled symbol macro function to add to tracked lexical environment
          ;;            (iter:collect `((symbol ,name) . ,replacer))))
          ;;        *screamer-macroexpansions*))
          ;; Expand the body with the new lexical macroexpansions
          `(let nil ,@(mapcar (compose #'first (rcurry #'expand-lexical-environments environment)) body)))
         ;; If an ordinary macro, expand it step by step
         ;; NOTE: Shouldn't be needed since walk calls this at every step, and
         ;; walk also expands macros when it finds them
         ;; ((guard (list* form-name _)
         ;;         (and
         ;;          (print (list 'name form-name))
         ;;          (symbolp (first form))
         ;;          (valid-macro? form environment)
         ;;          (not (equal form (macroexpand-1 form environment)))
         ;;          ))
         ;;  (let ((*macroexpand-hook* #'funcall))
         ;;    (print (list "expanding-macro" form (macroexpand form environment)))
         ;;    (call-expansion (macroexpand form environment) environment)))
         ;; If a known lexical macro, expand it and then look at the result
         ((guard (list* form-name form-args)
                 (assoc form-name *screamer-macroexpansions*))
          (let* ((match (assoc form-name *screamer-macroexpansions*))
                 ;; Get the macroexpansion function
                 (converter (cdr match)))
            (declare (function converter))
            ;; (print '(known-macro))
            ;; Call the macroexpansion function on the args and look at the result
            (call-expansion (apply converter form-args) environment)))
         ;; If a known lexical symbol-macro, expand it and look at the result
         ((guard (type symbol)
                 (assoc `(symbol ,form) *screamer-macroexpansions* :test 'equal))
          (let* ((match (assoc (list 'symbol form) *screamer-macroexpansions* :test 'equal))
                 ;; Get the macroexpansion function
                 (converter (cdr match)))
            (declare (function converter))
            ;; (print `(known-symbol ,form match ,match))
            (call-expansion (funcall converter) environment)))
         ;; If the form doesn't match the lexical expansion cases, walk the form.
         ;; Note that `walk' (seemingly?) relies on the map-function to call it
         ;; recursively when lacking a `reduce-function', rather than calling itself,
         ;; so the below doesn't create multiple nested recursive processes or similar.
         ;; (and (print "surrendering") (walk walk-map-function nil t nil nil form environment))
         (_ form))
       *screamer-macroexpansions*))))

(defun-compile-time walk
    (map-function reduce-function screamer? partial? nested? form environment)
  (declare
   (optimize (speed 3) (space 3))
   (type (or function null) map-function reduce-function))
  ;; TODO: Add MACROLET walking (via trivial-environments since this can't
  ;; otherwise be done portably, with a fallback to fail on this case?)
  ;; needs work: Cannot walk MACROLET or special forms not in both CLtL1 and
  ;;             CLtL2.
  (destructuring-bind (form *screamer-macroexpansions*)
      (expand-lexical-environments form environment)
    ;; (print (list 'expansion-done 'form form 'expansions-known *screamer-macroexpansions*))
    (cond
      ((self-evaluating? form) (funcall map-function form 'quote))
      ((symbolp form) (funcall map-function form 'variable))

      ((eq (first form) 'block)
       (walk-block
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'catch)
       (walk-catch
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'eval-when)
       (walk-eval-when
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'flet)
       (walk-flet/labels
        map-function reduce-function screamer? partial? nested? form environment
        'flet))
      ((eq (first form) 'function)
       (walk-function
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'go) (walk-go map-function form))
      ((eq (first form) 'if)
       (walk-if map-function reduce-function screamer? partial? nested? form
                environment))
      ((eq (first form) 'labels)
       (walk-flet/labels
        map-function reduce-function screamer? partial? nested? form environment
        'labels))
      ((eq (first form) 'let)
       (walk-let/let*
        map-function reduce-function screamer? partial? nested? form environment
        'let))
      ((eq (first form) 'let*)
       (walk-let/let*
        map-function reduce-function screamer? partial? nested? form environment
        'let*))
      ;; needs work: This is a temporary kludge to support MCL.
      ((and (eq (first form) 'locally) (null (fourth form)))
       (walk map-function reduce-function screamer? partial? nested? (third form)
             environment))
      ((eq (first form) 'multiple-value-call)
       (walk-multiple-value-call
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'multiple-value-prog1)
       (walk-multiple-value-prog1
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'progn)
       (walk-progn
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'progv)
       (walk-progv
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'quote) (walk-quote map-function form))
      ((eq (first form) 'return-from)
       (walk-return-from
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'setq)
       (walk-setq
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'tagbody)
       (walk-tagbody
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'the)
       (walk-the
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'throw)
       (walk-throw
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'unwind-protect)
       (walk-unwind-protect
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'handler-bind)
       (walk-handler-bind
        map-function reduce-function screamer? partial? nested? form environment))
      ((eq (first form) 'handler-case)
       (walk-handler/restart-case
        map-function reduce-function screamer? partial? nested? form environment
        'handler-case))
      ((eq (first form) 'restart-case)
       (walk-handler/restart-case
        map-function reduce-function screamer? partial? nested? form environment
        'restart-case))
      ((eq (first form) 'ignore-errors)
       (walk-ignore-errors
        map-function reduce-function screamer? partial? nested? form environment))
      ((and screamer? (eq (first form) 'for-effects))
       (walk-for-effects
        map-function reduce-function screamer? partial? nested? form environment))
      ((and screamer? (eq (first form) 'setf))
       (walk-setf
        map-function reduce-function screamer? partial? nested? form environment))
      ((and screamer? (eq (first form) 'local))
       (let ((*local?* t))
         (walk-progn
          map-function reduce-function screamer? partial? nested? form
          environment)))
      ((and screamer? (eq (first form) 'global))
       (let ((*local?* nil))
         (walk-progn
          map-function reduce-function screamer? partial? nested? form
          environment)))
      ((and screamer? (eq (first form) 'multiple-value-call-nondeterministic))
       (walk-multiple-value-call-nondeterministic
        map-function reduce-function screamer? partial? nested? form environment))
      ((and partial? (eq (first form) 'full)) (walk-full map-function form))

      ;; Dealing with macros
      ((valid-macro? form environment)
       ;; Walk macro call
       (walk-macro-call
        map-function reduce-function screamer? partial? nested? form environment))

      ((special-operator-p (first form))
       (error "Cannot (currently) handle the special form ~S" (first form)))

      (t (walk-function-call
          map-function reduce-function screamer? partial? nested? form
          environment)))))

(defun-compile-time process-subforms (function form form-type environment)
  (case form-type
    (lambda-list (error "This shouldn't happen"))
    ((variable go) form)
    ((eval-when)
     (cached-list* (first form)
                   (second form)
                   (mapcar #'(lambda (subform)
                               (funcall function subform environment))
                           (rest (rest form)))))
    ((flet labels)
     `(,(first form)
       ,(mapcar
         #'(lambda (binding)
             (cl:multiple-value-bind (body declarations documentation-string)
                 (peal-off-documentation-string-and-declarations
                  (rest (rest binding)) t)
               `(,(first binding)
                 ;; TODO: Fix to process subforms of lambda list.
                 ,(second binding)
                 ,@(if documentation-string (cached-list documentation-string))
                 ,@declarations
                 ,@(mapcar
                    #'(lambda (subform) (funcall function subform environment))
                    body))))
         (second form))
       ,@(mapcar
          #'(lambda (subform) (funcall function subform environment))
          (rest (rest form)))))
    ((let let*)
     (cl:multiple-value-bind (body declarations)
         (peal-off-documentation-string-and-declarations (rest (rest form)))
       `(,(first form)
         ,(mapcar
           #'(lambda (binding)
               (if (and (consp binding) (= (length binding) 2))
                   `(,(first binding)
                     ,(funcall function (second binding) environment))
                   binding))
           (second form))
         ,@declarations
         ,@(mapcar
            #'(lambda (subform) (funcall function subform environment)) body))))
    (progn
      `(progn ,@(mapcar
                 #'(lambda (subform) (funcall function subform environment))
                 (rest form))))
    (quote (quotify form))
    (the `(the ,(second form) ,(funcall function (third form) environment)))
    (macro-call (error "This shouldn't happen"))
    (lambda-call
     (cl:multiple-value-bind (body declarations documentation-string)
         (peal-off-documentation-string-and-declarations
          (rest (rest (first form))) t)
       ;; TODO: Fix to process subforms of lambda list.
       `((lambda ,(second (first form))
           ,@(if documentation-string (list documentation-string))
           ,@declarations
           ,@(mapcar #'(lambda (subform) (funcall function subform environment))
                     body))
         ,@(mapcar
            #'(lambda (subform) (funcall function subform environment))
            (rest form)))))
    (otherwise
     (cached-cons (first form)
                  (mapcar #'(lambda (subform) (funcall function subform environment))
                          (rest form))))))

(defun-compile-time deterministic? (form environment)
  (walk
   #'(lambda (form form-type)
       (case form-type
         ((symbol-call setf-call)
          (function-record-deterministic? (get-function-record (first form))))
         (multiple-value-call-nondeterministic nil)
         ;; NOTE: not really sure about CATCH, THROW and UNWIND-PROTECT
         (otherwise t)))
   ;; NOTE: potentially inefficient because must walk entire form even
   ;;       after it is known to be nondeterministic
   #'(lambda (&optional (x nil x?) y) (if x? (and x y) t))
   t
   nil
   nil
   form
   environment))

(defun-compile-time deterministic-lambda-list? (lambda-list environment)
  (walk-lambda-list
   #'(lambda (form form-type)
       (case form-type
         ((symbol-call setf-call)
          (function-record-deterministic? (get-function-record (first form))))
         (multiple-value-call-nondeterministic nil)
         ;; NOTE: not really sure about CATCH, THROW and UNWIND-PROTECT
         (otherwise t)))
   ;; NOTE: potentially inefficient because must walk entire form even
   ;;       after it is known to be nondeterministic
   #'(lambda (&optional (x nil x?) y) (if x? (and x y) t))
   t
   nil
   nil
   lambda-list
   environment))

(defun-compile-time needs-substitution? (form environment)
  (walk
   #'(lambda (form form-type)
       (case form-type
         (function-lambda
          (not (and (every #'(lambda (form) (deterministic? form environment))
                           (peal-off-documentation-string-and-declarations
                            (rest (rest (second form))) t))
                    (deterministic-lambda-list?
                     (second (second form)) environment))))
         ((function-symbol function-setf)
          (not (function-record-deterministic?
                (get-function-record (second form)))))
         (return-from (let ((tag (assoc (second form) *block-tags* :test #'eq)))
                        (and tag (second tag))))
         (go (let ((tag (assoc (second form) *tagbody-tags*)))
               (and tag (second tag))))
         (setq *local?*)
         (local-setf t)
         (otherwise nil)))
   ;; NOTE: potentially inefficient because must walk entire form even
   ;;       after it is known to need substitution
   #'(lambda (&optional (x nil x?) y) (if x? (or x y) '()))
   t
   nil
   t
   form
   environment))

(defun-compile-time contains-local-setf/setq? (form environment)
  (walk #'(lambda (form form-type)
            (declare (ignore form))
            (or (and *local?* (eq form-type 'setq))
                (eq form-type 'local-setf)))
        ;; NOTE: potentially inefficient because must walk entire form even
        ;;       after it is known to contain a LOCAL SETF/SETQ special form
        #'(lambda (&optional (x nil x?) y) (if x? (or x y) '()))
        t
        nil
        nil
        form
        environment))

(defun-compile-time form-callees (form environment)
  (walk #'(lambda (form form-type)
            (case form-type
              ((function-symbol function-setf) (list (second form)))
              ((symbol-call setf-call) (list (first form)))
              (otherwise '())))
        #'(lambda (&optional (x nil x?) y)
            (if x? (union x y :test #'equal) '()))
        t
        nil
        t
        form
        environment))

(defun-compile-time callees (function-name)
  (function-record-callees (get-function-record function-name)))

(defun-compile-time indirect-callees-internal (function-names callees)
  (if (null function-names)
      callees
      (let ((function-name (first function-names)))
        (if (member function-name callees :test #'equal)
            (indirect-callees-internal (rest function-names) callees)
            (indirect-callees-internal
             (rest function-names)
             (indirect-callees-internal
              (callees function-name) (cached-cons function-name callees)))))))

(defun-compile-time indirect-callees (function-name)
  (indirect-callees-internal (callees function-name) '()))

(defun-compile-time callers (function-name)
  (let ((callers '())
        (function-names '()))
    (maphash #'(lambda (function-name function-record)
                 (declare (ignore function-record))
                 (cached-push function-name function-names))
             *function-record-table*)
    (dolist (caller function-names)
      (if (member function-name (callees caller) :test #'equal)
          (pushnew caller callers :test #'equal)))
    callers))

(defun-compile-time indirect-callers-internal (function-names callers)
  (if (null function-names)
      callers
      (let ((function-name (first function-names)))
        (if (member function-name callers :test #'equal)
            (indirect-callers-internal (rest function-names) callers)
            (indirect-callers-internal
             (rest function-names)
             (indirect-callers-internal
              (callers function-name) (cached-cons function-name callers)))))))

(defun-compile-time indirect-callers (function-name)
  (indirect-callers-internal (callers function-name) '()))

(defun-compile-time expand-local-setf (pairs environment)
  (if (null pairs)
      '(progn)
      (let ((d (gensym "DUMMY-"))
            (dummy-argument (gensym "DUMMY-")))
        (cl:multiple-value-bind (vars vals stores store-form access-form)
            (get-setf-expansion (first pairs) environment)
          `(let* (,@(mapcar #'list vars vals)
                  (,dummy-argument ,(second pairs))
                  (,d ,access-form))
             (trail #'(lambda () ,(subst d (first stores) store-form)))
             ,@(if (null (rest (rest pairs)))
                   (list (subst dummy-argument (first stores) store-form))
                   (list (subst dummy-argument (first stores) store-form)
                         (expand-local-setf (rest (rest pairs)) environment))))))))

(defun-compile-time expand-local-setq (pairs environment)
  (if (null pairs)
      '(progn)
      (let ((d (gensym "DUMMY-")))
        `(let ((,d ,(first pairs)))
           (trail #'(lambda () (setq ,(first pairs) ,d)))
           ,@(if (null (rest (rest pairs)))
                 (list `(setq
                         ,(first pairs)
                         ,(perform-substitutions (second pairs) environment)))
                 (list `(setq
                         ,(first pairs)
                         ,(perform-substitutions (second pairs) environment))
                       (expand-local-setq (rest (rest pairs)) environment)))))))

(defun-compile-time perform-substitutions (form environment)
  (if (needs-substitution? form environment)
      (walk
       #'(lambda (form form-type)
           (case form-type
             (lambda-list (error "This shouldn't happen"))
             (variable (error "This shouldn't happen"))
             (block (let ((*block-tags*
                            (cached-cons (list (second form) nil) *block-tags*)))
                      (process-subforms
                       #'perform-substitutions form form-type environment)))
             (function-lambda
              (unless (deterministic-lambda-list?
                       (second (second form)) environment)
                (screamer-error
                 "Cannot (currently) handle a LAMDBA expression with~%~
              nondeterministic initializations forms for~%~
              &OPTIONAL and &AUX parameters: ~S"
                 form))
              (cl:multiple-value-bind (body declarations documentation-string)
                  (peal-off-documentation-string-and-declarations
                   (rest (rest (second form))) t)
                (if (every #'(lambda (form) (deterministic? form environment))
                           body)
                    ;; TODO: Fix to process subforms of lambda list.
                    `#'(lambda ,(second (second form))
                         ,@(if documentation-string (list documentation-string))
                         ,@declarations
                         ,@(mapcar
                            #'(lambda (subform)
                                (perform-substitutions subform environment))
                            body))
                    (let ((continuation (gensym "CONTINUATION-")))
                      ;; NOTE: This conses every time #'(LAMBDA (...) ...) is
                      ;;       accessed when it is nondeterministic. A small
                      ;;       price to pay for a lot of error checking.
                      `(make-nondeterministic-function
                        :function
                        ;; TODO: Fix to process subforms of lambda list.
                        #'(lambda (,continuation ,@(second (second form)))
                            ,@(if documentation-string (list documentation-string))
                            ,@declarations
                            ,continuation ;ignore
                            ,(cps-convert-progn body
                                                continuation
                                                '()
                                                t
                                                environment)))))))
             ((function-symbol function-setf)
              (if (function-record-deterministic?
                   (get-function-record (second form)))
                  form
                  ;; NOTE: This conses every time #'FOO  or #'(SETF FOO) is
                  ;;       accessed when FOO or (SETF FOO) is nondeterministic.
                  ;;       A small price to pay for a lot of error checking.
                  `(make-nondeterministic-function
                    :function #',(cps-convert-function-name (second form)))))
             (go (let ((tag (assoc (second form) *tagbody-tags*)))
                   ;; NOTE: Can't issue an error here if tag not found since it
                   ;;       might be outside the scope of a FOR-EFFECTS.
                   (if (and tag (second tag)) `(,(second tag)) form)))
             (quote (error "This shouldn't happen"))
             (return-from
              (let ((tag (assoc (second form) *block-tags* :test #'eq))
                    (value (perform-substitutions
                            (if (= (length form) 3) (third form) nil)
                            environment)))
                ;; NOTE: Can't issue an error here if tag not found since it
                ;;       might be outside the scope of a FOR-EFFECTS.
                (if (and tag (second tag))
                    (possibly-beta-reduce-funcall
                     (second tag) '() value (fourth tag))
                    `(return-from ,(second form) ,value))))
             (setq (if *local?*
                       (expand-local-setq (rest form) environment)
                       (process-subforms
                        #'perform-substitutions form form-type environment)))
             (tagbody (let ((*tagbody-tags*
                              (append (mapcar #'(lambda (tag) (list tag nil))
                                              (remove-if #'consp (rest form)))
                                      *tagbody-tags*)))
                        (process-subforms
                         #'perform-substitutions form form-type environment)))
             (for-effects (perform-substitutions
                           (let ((*macroexpand-hook* #'funcall))
                             (macroexpand-1 form environment))
                           environment))
             (local-setf (perform-substitutions
                          (expand-local-setf (rest form) environment)
                          environment))
             (macro-call (error "This shouldn't happen"))
             (otherwise (process-subforms
                         #'perform-substitutions form form-type environment))))
       nil
       t
       nil
       nil
       form
       environment)
      form))

(defun-compile-time is-magic-declaration? (form)
  (and (consp form)
       (eq (first form) 'declare)
       (consp (rest form))
       (consp (second form))
       (eq (first (second form)) 'magic)))

(defun-compile-time is-magic-continuation? (continuation)
  ;; Checks that CONTINUATION is of the form:
  ;;   #'(lambda (...) (declare (magic) ...) ...)
  (and (consp continuation)
       (eq (first continuation) 'function)
       (null (rest (last continuation)))
       (= (length continuation) 2)
       (lambda-expression? (second continuation))
       (>= (length (second continuation)) 3)
       (is-magic-declaration? (third (second continuation)))))

(defun-compile-time magic-continuation-argument (continuation)
  (if (or (eq (first (second (second continuation))) '&optional)
          (eq (first (second (second continuation))) '&rest))
      (second (second (second continuation)))
      (first (second (second continuation)))))

(defun-compile-time possibly-beta-reduce-funcall
    (continuation types form value?)
  (unless (or (and (symbolp continuation) (not (symbol-package continuation)))
              (and (consp continuation)
                   (eq (first continuation) 'function)
                   (null (rest (last continuation)))
                   (= (length continuation) 2)
                   (symbolp (second continuation)))
              (is-magic-continuation? continuation))
    (error "Please report this bug; This shouldn't happen (A)"))
  (cond
    ((symbolp continuation)
     (if value?
         (if (null types)
             (if (consp form)
                 `(multiple-value-call ,continuation ,form)
                 ;; NOTE: This optimization is technically unsound if FORM
                 ;;       is a symbol macro that returns multiple values.
                 `(funcall ,continuation ,form))
             ;; NOTE: This optimization assumes that there are no VALUES
             ;;       types.
             `(funcall ,continuation (the (and ,@types) ,form)))
         `(progn ,form (funcall ,continuation))))
    ((symbolp (second continuation))
     (if value?
         (if (null types)
             (if (consp form)
                 `(multiple-value-call ,continuation ,form)
                 ;; NOTE: This optimization is technically unsound if FORM
                 ;;       is a symbol macro that returns multiple values.
                 `(,(second continuation) ,form))
             ;; NOTE: This optimization assumes that there are no VALUES
             ;;       types.
             `(,(second continuation) (the (and ,@types) ,form)))
         `(progn ,form (,(second continuation)))))
    (t (if value?
           (progn
             (if (null (second (second continuation)))
                 (error "Please report this bug; This shouldn't happen (B)"))
             (cond
               ((eq (first (second (second continuation))) '&rest)
                (if (null types)
                    `(let ((,(magic-continuation-argument continuation)
                             (multiple-value-list ,form)))
                       ;; Peal off LAMBDA, arguments, and DECLARE.
                       ,@(rest (rest (rest (second continuation)))))
                    `(let ((,(magic-continuation-argument continuation)
                             (list (the (and ,@types) ,form))))
                       ;; Peal off LAMBDA, arguments, and DECLARE.
                       ,@(rest (rest (rest (second continuation)))))))
               ((or (and (consp form)
                         (not
                          (and (eq (first form) 'function)
                               (null (rest (last form)))
                               (= (length form) 2)
                               (symbolp (second form)))))
                    (and (symbolp form) (symbol-package form))
                    (symbol-package (magic-continuation-argument continuation)))
                (if (null types)
                    `(let ((,(magic-continuation-argument continuation) ,form))
                       (declare
                        (dynamic-extent
                         ,(magic-continuation-argument continuation)))
                       ;; Peal off LAMBDA, arguments, and DECLARE.
                       ,@(rest (rest (rest (second continuation)))))
                    `(let ((,(magic-continuation-argument continuation)
                             (the (and ,@types) ,form)))
                       (declare
                        (type (and ,@types)
                              ,(magic-continuation-argument continuation)))
                       ;; Peal off LAMBDA, arguments, and DECLARE.
                       ,@(rest (rest (rest (second continuation)))))))
               ;; NOTE: This case may be unsoundly taken in the following cases:
               ;;       a. (MAGIC-CONTINUATION-ARGUMENT CONTINUATION) is a
               ;;          non-Screamer GENSYM. This can only happen if a
               ;;          a BINDING-VARIABLE is a GENSYM in CPS-CONVERT-LET*.
               ;;       b. FORM is a non-Screamer GENSYM
               (t (if (null types)
                      (subst form
                             (magic-continuation-argument continuation)
                             ;; Peal off LAMBDA, arguments, and DECLARE.
                             `(progn ,@(rest (rest (rest (second continuation)))))
                             :test #'eq)
                      (subst `(the (and ,@types) ,form)
                             (magic-continuation-argument continuation)
                             ;; Peal off LAMBDA, arguments, and DECLARE.
                             `(progn ,@(rest (rest (rest (second continuation)))))
                             :test #'eq)))))
           (progn
             (unless (null (second (second continuation)))
               (error "Please report this bug; This shouldn't happen (C)"))
             ;; Peal off LAMBDA, arguments, and DECLARE.
             `(progn ,form ,@(rest (rest (rest (second continuation))))))))))

(defun-compile-time void-continuation (continuation)
  (unless (or (and (symbolp continuation) (not (symbol-package continuation)))
              (and (consp continuation)
                   (eq (first continuation) 'function)
                   (null (rest (last continuation)))
                   (= (length continuation) 2)
                   (symbolp (second continuation)))
              (is-magic-continuation? continuation))
    (error "Please report this bug; This shouldn't happen (D)"))
  (let ((dummy-argument (gensym "DUMMY-")))
    ;; NOTE: We could get rid of this bogosity by having two versions of each
    ;;       nondeterministic function, one which returned a value and one which
    ;;       didn't.
    `#'(lambda (&rest ,dummy-argument)
         (declare (magic)
                  (ignore ,dummy-argument))
         ,@(cond ((symbolp continuation) `((funcall ,continuation)))
                 ((symbolp (second continuation)) `((,(second continuation))))
                 ;; Peal off LAMBDA, arguments, and DECLARE.
                 (t (rest (rest (rest (second continuation)))))))))

(defun-compile-time cps-convert-function-name (function-name)
  (if (symbolp function-name)
      (intern (format nil "~A-NONDETERMINISTIC" (string function-name))
              (symbol-package function-name))
      `(setf ,(intern (format nil "~A-NONDETERMINISTIC"
                              (string (second function-name)))
                      (symbol-package (second function-name))))))

(defun-compile-time cps-convert-block
    (name body continuation types value? environment)
  (let* ((c (gensym "CONTINUATION-"))
         (*block-tags* (cached-cons (list name c types value?) *block-tags*)))
    (possibly-beta-reduce-funcall
     `#'(lambda (,c)
          (declare (magic))
          ,(cps-convert-progn body c types value? environment))
     '()
     continuation
     t)))

(defun-compile-time cps-convert-if (antecedent
                                    consequent
                                    alternate
                                    continuation
                                    types
                                    value?
                                    environment)
  (let ((c (gensym "CONTINUATION-"))
        (dummy-argument (gensym "DUMMY-"))
        (other-arguments (gensym "OTHER-")))
    (possibly-beta-reduce-funcall
     `#'(lambda (,c)
          (declare (magic))
          ,(cps-convert
            antecedent
            `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
                 (declare (magic)
                          (ignore ,other-arguments))
                 (if ,dummy-argument
                     ,(cps-convert consequent c types value? environment)
                     ,(cps-convert alternate c types value? environment)))
            '()
            t
            environment))
     '()
     continuation
     t)))

(defun-compile-time cps-convert-let (bindings
                                     body
                                     declarations
                                     continuation
                                     types
                                     value?
                                     environment
                                     &optional
                                     new-bindings)
  (if (null bindings)
      `(let ,new-bindings
         ,@declarations
         ,(cps-convert-progn body continuation types value? environment))
      (let* ((binding (first bindings))
             (binding-variable
               (if (symbolp binding) binding (first binding)))
             (binding-form
               (if (and (consp binding) (= (length binding) 2))
                   (second binding)
                   nil))
             (dummy-argument (gensym "DUMMY-"))
             (other-arguments (gensym "OTHER-")))
        (cps-convert
         binding-form
         `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments))
              ,(cps-convert-let (rest bindings)
                                body
                                declarations
                                continuation
                                types
                                value?
                                environment
                                (cached-cons (list binding-variable dummy-argument)
                                             new-bindings)))
         '()
         t
         environment))))

(defun-compile-time cps-convert-let* (bindings
                                      body
                                      declarations
                                      continuation
                                      types
                                      value?
                                      environment)
  (if (null bindings)
      (if (null declarations)
          (cps-convert-progn body continuation types value? environment)
          `(let ()
             ,@declarations
             ,(cps-convert-progn body continuation types value? environment)))
      (let* ((binding (first bindings))
             (binding-variable
               (if (symbolp binding) binding (first binding)))
             (binding-form
               (if (and (consp binding) (= (length binding) 2))
                   (second binding)
                   nil))
             (other-arguments (gensym "OTHER-")))
        (cps-convert
         binding-form
         `#'(lambda (&optional ,binding-variable &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments))
              ,(cps-convert-let* (rest bindings)
                                 body
                                 declarations
                                 continuation
                                 types
                                 value?
                                 environment))
         '()
         t
         environment))))

(defun-compile-time cps-convert-multiple-value-call-internal
    (nondeterministic? function forms continuation types value? environment
                       &optional arguments)
  (if (null forms)
      (if nondeterministic?
          ;; needs work: TYPES is never actually used in this branch.
          `(apply-nondeterministic-nondeterministic
            ,(if value? continuation (void-continuation continuation))
            ,function
            (append ,@(reverse arguments)))
          (possibly-beta-reduce-funcall
           continuation
           types
           `(apply ,function (append ,@(reverse arguments)))
           value?))
      (let ((dummy-argument (gensym "DUMMY-")))
        (cps-convert
         (first forms)
         `#'(lambda (&rest ,dummy-argument)
              (declare (magic))
              ,(cps-convert-multiple-value-call-internal
                nondeterministic? function (rest forms) continuation types value?
                environment (cached-cons dummy-argument arguments)))
         nil
         t
         environment))))

(defun-compile-time cps-convert-multiple-value-call
    (nondeterministic? function forms continuation types value? environment)
  (let ((dummy-argument (gensym "DUMMY-"))
        (other-arguments (gensym "OTHER-")))
    (cps-convert
     function
     `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
          (declare (magic)
                   (ignore ,other-arguments))
          ,(cps-convert-multiple-value-call-internal
            nondeterministic? dummy-argument forms continuation types value?
            environment))
     nil
     t
     environment)))

(defun-compile-time cps-convert-multiple-value-prog1
    (form forms continuation types value? environment)
  (if value?
      (let ((dummy-argument (gensym "DUMMY-")))
        (cps-convert
         form
         `#'(lambda (&rest ,dummy-argument)
              (declare (magic))
              ,(cps-convert-progn
                forms
                `#'(lambda ()
                     (declare (magic))
                     (possibly-beta-reduce-funcall
                      continuation types `(values-list ,dummy-argument) t))
                nil
                nil
                environment))
         types
         t
         environment))
      (cps-convert-progn (cached-cons form forms) continuation types nil environment)))

(defun-compile-time cps-convert-progn
    (body continuation types value? environment)
  (cond
    ((null body) (possibly-beta-reduce-funcall continuation types nil value?))
    ((null (rest body))
     (cps-convert (first body) continuation types value? environment))
    (t (cps-convert
        (first body)
        `#'(lambda ()
             (declare (magic))
             ,(cps-convert-progn
               (rest body) continuation types value? environment))
        '()
        nil
        environment))))

(defun-compile-time cps-convert-return-from (name result environment)
  (let ((tag (assoc name *block-tags* :test #'eq)))
    (if (and tag (second tag))
        (cps-convert result (second tag) (third tag) (fourth tag) environment)
        ;; NOTE: Can't issue an error here if tag not found since it might be
        ;;       outside the scope of a FOR-EFFECTS. Thus we must compile a
        ;;       RETURN-FROM nondeterministic code to deterministic code.
        ;;       Likewise, can't issue an error here if tag is found but
        ;;       (SECOND TAG) is NIL since this arrises when you have a
        ;;       RETURN-FROM inside a FOR-EFFECTS to a tag outside the
        ;;       FOR-EFFECTS.
        (let ((dummy-argument (gensym "DUMMY-")))
          (cps-convert
           result
           `#'(lambda (&rest ,dummy-argument)
                (declare (magic))
                (return-from ,name (values-list ,dummy-argument)))
           '()
           t
           environment)))))

(defun-compile-time cps-convert-setq
    (arguments continuation types value? environment)
  (if (null arguments)
      (possibly-beta-reduce-funcall continuation types nil value?)
      (let ((dummy-argument (gensym "DUMMY-"))
            (other-arguments (gensym "OTHER-")))
        (cps-convert
         (second arguments)
         `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments)
                       ,@(if (and (null (rest (rest arguments)))
                                  (not (null types)))
                             `((type (and ,@types) ,dummy-argument))))
              ,(if (null (rest (rest arguments)))
                   (possibly-beta-reduce-funcall
                    continuation
                    types
                    `(setq ,(first arguments) ,dummy-argument)
                    value?)
                   `(progn (setq ,(first arguments) ,dummy-argument)
                           ,(cps-convert-setq
                             (rest (rest arguments))
                             continuation
                             types
                             value?
                             environment))))
         (if (null (rest (rest arguments))) types '())
         t
         environment))))

(defun-compile-time cps-convert-tagbody
    (body continuation types value? environment)
  (s:nest
   (let ((segments (list (list 'header)))
         (*tagbody-tags* *tagbody-tags*)) ;cool!
     (dolist (form body)
       (if (consp form)
           (cached-push form (rest (first segments)))
           (let ((c (gensym "CONTINUATION-")))
             (cached-push (list form c) *tagbody-tags*)
             (cached-push (list c) segments))))
     (cached-push nil (rest (first segments))))
   (let ((segments (reverse segments))
         (dummy-argument (gensym "DUMMY-"))
         (other-arguments (gensym "OTHER-"))))
   `(labels ,(mapcar
              #'(lambda (segment)
                  (let ((next (rest (member segment segments :test #'eq))))
                    `(,(first segment)
                      (&optional ,dummy-argument &rest ,other-arguments)
                      (declare (ignore ,dummy-argument ,other-arguments))
                      ,(cps-convert-progn
                        (reverse (rest segment))
                        (if next `#',(first (first next)) continuation)
                        (if next '() types)
                        (or next value?)
                        environment))))
              (rest segments))
      (declare (dynamic-extent
                ,@(mapcar (lambda (seg) `(function ,(first seg)))
                          (rest segments))))
      ,(let ((next (rest segments)))
         (cps-convert-progn
          (reverse (rest (first segments)))
          (if next `#',(first (first next)) continuation)
          (if next '() types)
          (or next value?)
          environment)))))

(defun-compile-time cps-convert-local-setf/setq
    (arguments continuation types value? environment)
  (if (null arguments)
      (possibly-beta-reduce-funcall continuation types nil value?)
      (let ((d (gensym "DUMMY-"))
            (dummy-argument (gensym "DUMMY-"))
            (other-arguments (gensym "OTHER-")))
        (cl:multiple-value-bind (vars vals stores store-form access-form)
            (get-setf-expansion (first arguments) environment)
          (cps-convert
           (second arguments)
           `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
                (declare (magic)
                         (ignore ,other-arguments)
                         ,@(if (and (null (rest (rest arguments)))
                                    (not (null types)))
                               `((type (and ,@types) ,dummy-argument))))
                (let* (,@(mapcar #'list vars vals) (,d ,access-form))
                  (unwind-protect
                       ,(if (null (rest (rest arguments)))
                            (possibly-beta-reduce-funcall
                             continuation
                             types
                             (subst dummy-argument (first stores) store-form)
                             value?)
                            `(progn ,(subst
                                      dummy-argument
                                      (first stores)
                                      store-form)
                                    ,(cps-convert-local-setf/setq
                                      (rest (rest arguments))
                                      continuation
                                      types
                                      value?
                                      environment)))
                    ,(subst d (first stores) store-form))))
           (if (null (rest (rest arguments))) types '())
           t
           environment)))))

(defun-compile-time cps-convert-call (function-name
                                      arguments
                                      continuation
                                      types
                                      value?
                                      environment
                                      &optional
                                      dummy-arguments)
  ;; needs work: TYPES is never actually used here.
  (if (null arguments)
      (let ((c (gensym "CONTINUATION-")))
        (possibly-beta-reduce-funcall
         `#'(lambda (,c)
              (declare (magic))
              (,(cps-convert-function-name function-name)
               ,c
               ,@(reverse dummy-arguments)))
         '()
         (if value? continuation (void-continuation continuation))
         t))
      (let ((dummy-argument (gensym "DUMMY-"))
            (other-arguments (gensym "OTHER-")))
        (cps-convert
         (first arguments)
         `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments))
              ,(cps-convert-call
                function-name
                (rest arguments)
                continuation
                types
                value?
                environment
                (cached-cons dummy-argument dummy-arguments)))
         '()
         t
         environment))))

(defun-compile-time cps-non-convert-call (function-name
                                          arguments
                                          continuation
                                          types
                                          value?
                                          environment
                                          &optional
                                          dummy-arguments)
  (if (null arguments)
      (possibly-beta-reduce-funcall
       continuation
       types
       (if (not (null types))
           `(the (and ,@types) (,function-name ,@(reverse dummy-arguments)))
           `(,function-name ,@(reverse dummy-arguments)))
       value?)
      (let ((dummy-argument (gensym "DUMMY-"))
            (other-arguments (gensym "OTHER-")))
        (cps-convert
         (first arguments)
         `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments))
              ,(cps-non-convert-call
                function-name
                (rest arguments)
                continuation
                types
                value?
                environment
                (cached-cons dummy-argument dummy-arguments)))
         '()
         t
         environment))))

(defun-compile-time cps-convert (form continuation types value? environment)
  (walk #'(lambda (form form-type)
            (if (and (not (eq form-type 'quote))
                     (deterministic? form environment)
                     (not (contains-local-setf/setq? form environment)))
                (possibly-beta-reduce-funcall
                 continuation
                 types
                 (perform-substitutions form environment)
                 value?)
                (case form-type
                  (lambda-list (error "This shouldn't happen"))
                  (variable (possibly-beta-reduce-funcall
                             continuation types form value?))
                  (block (cps-convert-block (second form)
                                            (rest (rest form))
                                            continuation
                                            types
                                            value?
                                            environment))
                  ((function-lambda function-symbol function-setf)
                   (possibly-beta-reduce-funcall
                    continuation
                    types
                    (perform-substitutions form environment)
                    value?))
                  (go (error "This shouldn't happen"))
                  (if (cps-convert-if (second form)
                                      (third form)
                                      (if (null (rest (rest (rest form))))
                                          nil
                                          (fourth form))
                                      continuation
                                      types
                                      value?
                                      environment))
                  (let (cl:multiple-value-bind (body declarations)
                           (peal-off-documentation-string-and-declarations
                            (rest (rest form)))
                         (cps-convert-let
                          (second form)
                          body
                          declarations
                          continuation
                          types
                          value?
                          environment)))
                  (let* (cl:multiple-value-bind (body declarations)
                            (peal-off-documentation-string-and-declarations
                             (rest (rest form)))
                          (cps-convert-let*
                           (second form)
                           body
                           declarations
                           continuation
                           types
                           value?
                           environment)))
                  (multiple-value-call
                      (cps-convert-multiple-value-call
                       nil
                       (second form)
                       (rest (rest form))
                       continuation
                       types
                       value?
                       environment))
                  (multiple-value-prog1
                      (cps-convert-multiple-value-prog1
                       (second form)
                       (rest (rest form))
                       continuation
                       types
                       value?
                       environment))
                  (progn (cps-convert-progn
                          (rest form) continuation types value? environment))
                  (quote (possibly-beta-reduce-funcall
                          continuation types (quotify form) value?))
                  (return-from (cps-convert-return-from
                                (second form)
                                (if (= (length form) 2) nil (third form))
                                environment))
                  (setq (if *local?*
                            (cps-convert-local-setf/setq
                             (rest form) continuation types value? environment)
                            (cps-convert-setq
                             (rest form) continuation types value? environment)))
                  (tagbody (cps-convert-tagbody
                            (rest form) continuation types value? environment))
                  (the (cps-convert (third form)
                                    continuation
                                    (cached-cons (second form) types)
                                    value?
                                    environment))
                  (for-effects (possibly-beta-reduce-funcall
                                continuation types form value?))
                  (local-setf
                   (cps-convert-local-setf/setq
                    (rest form) continuation types value? environment))
                  (multiple-value-call-nondeterministic
                   (cps-convert-multiple-value-call
                    t
                    (second form)
                    (rest (rest form))
                    continuation
                    types
                    value?
                    environment))
                  (macro-call (error "This shouldn't happen"))
                  (lambda-call
                   (unless (deterministic-lambda-list?
                            (second (first form)) environment)
                     (screamer-error
                      "Cannot (currently) handle a LAMDBA expression with~%~
                   nondeterministic initializations forms for~%~
                   &OPTIONAL and &AUX parameters: ~S"
                      form))
                   (unless (every
                            #'(lambda (argument)
                                (and (symbolp argument)
                                     (not (member argument lambda-list-keywords
                                                  :test #'eq))))
                            (second (first form)))
                     (error "Cannot (currently) handle a nondeterministic~%~
                         form whose CAR is a LAMBDA expression with~%~
                         lambda list keywords or arguments that are not~%~
                         symbols: ~S"
                            form))
                   (unless (= (length (second (first form)))
                              (length (rest form)))
                     (error "The form ~S has a CAR which is a LAMBDA~%~
                         expression which takes a different number of~%~
                         arguments than it is called with"
                            form))
                   (cl:multiple-value-bind (body declarations)
                       (peal-off-documentation-string-and-declarations
                        (rest (rest (first form))) t)
                     ;; NOTE: The documentation string is lost for lambda calls
                     ;;       that are CPS Converted.
                     (cps-convert-let
                      (mapcar #'list (second (first form)) (rest form))
                      body
                      declarations
                      continuation
                      types
                      value?
                      environment)))
                  ((symbol-call setf-call)
                   (if (function-record-deterministic?
                        (get-function-record (first form)))
                       (cps-non-convert-call (first form)
                                             (rest form)
                                             continuation
                                             types
                                             value?
                                             environment)
                       (cps-convert-call (first form)
                                         (rest form)
                                         continuation
                                         types
                                         value?
                                         environment)))
                  (otherwise
                   (screamer-error
                    "Cannot (currently) handle the special form ~S inside a~%~
                  nondeterministic context."
                    (first form))))))
        nil
        t
        nil
        nil
        form
        environment))

(defun-compile-time declare-deterministic (function-name)
  (setf (function-record-deterministic? (get-function-record function-name)) t))

(defun-compile-time declare-nondeterministic (function-name)
  (setf (function-record-deterministic? (get-function-record function-name))
        nil))

(defun-compile-time compute-callees (body environment)
  ;; NOTE: What bogosity in Common Lisp! UNION should allow zero arguments and
  ;;       return NIL as the identity element for use by REDUCE.
  (reduce
   #'union
   (mapcar #'(lambda (form) (form-callees form environment))
           (peal-off-documentation-string-and-declarations body t))
   :initial-value '()))

(defun-compile-time cache-definition (function-name lambda-list body callees)
  (let ((function-record (get-function-record function-name)))
    (setf (function-record-lambda-list function-record) lambda-list)
    (setf (function-record-body function-record) body)
    (setf (function-record-callees function-record) callees)))

(defun-compile-time determine-whether-deterministic (function-name environment)
  ;; NOTE: This is using the current rather than the saved ENVIRONMENT.
  (let* ((function-record (get-function-record function-name)))
    (setf (function-record-deterministic? function-record)
          (and (every #'(lambda (form) (deterministic? form environment))
                      (peal-off-documentation-string-and-declarations
                       (function-record-body function-record) t))
               (deterministic-lambda-list?
                (function-record-lambda-list function-record) environment)))))

(defun-compile-time determine-whether-callers-are-deterministic
    (function-name function-names environment)
  ;; NOTE: This is using the current rather than the saved ENVIRONMENT.
  (dolist (caller (callers function-name))
    (unless (member caller function-names :test #'equal)
      (determine-whether-deterministic caller environment)
      (determine-whether-callers-are-deterministic
       caller (cached-cons caller function-names) environment))))

(defun-compile-time function-definition (function-name environment)
  ;; NOTE: This is using the current rather than the saved ENVIRONMENT.
  (let* ((function-record (get-function-record function-name))
         (lambda-list (function-record-lambda-list function-record))
         (body (function-record-body function-record)))
    (cl:multiple-value-bind (body declarations documentation-string)
        (peal-off-documentation-string-and-declarations body t)
      (if (function-record-deterministic? function-record)
          (let ((*block-tags* (list (list function-name nil))))
            ;; TODO: Fix to process subforms of lambda list.
            (list `(cl:defun ,function-name ,lambda-list
                     ,@(if documentation-string (list documentation-string))
                     ,@declarations
                     ,@(mapcar #'(lambda (form)
                                   (perform-substitutions form environment))
                               body))
                  `(declare-deterministic ',function-name)))
          (let* ((continuation (gensym "CONTINUATION-"))
                 ;; NOTE: Could provide better TYPES and VALUE? here.
                 (*block-tags* (list (list function-name continuation '() t))))
            (list `(cl:defun ,function-name ,lambda-list
                     ,@(if documentation-string (list documentation-string))
                     ,@declarations
                     (declare
                      (ignore
                       ,@(reduce
                          #'append
                          (mapcar
                           #'(lambda (argument)
                               (if (consp argument)
                                   (if (and (consp (rest argument))
                                            (consp (rest (rest argument))))
                                       (list (first argument) (third argument))
                                       (list (first argument)))
                                   (list argument)))
                           (set-difference
                            lambda-list
                            lambda-list-keywords
                            :test #'eq)))))
                     (screamer-error
                      "Function ~S is a nondeterministic function. As such, it~%~
                  must be called only from a nondeterministic context."
                      ',function-name))
                  `(cl:defun ,(cps-convert-function-name function-name)
                       (,continuation ,@lambda-list)
                     ,@(if documentation-string (list documentation-string))
                     ,@declarations
                     ,continuation      ;ignore
                     ,(cps-convert-progn body continuation '() t environment))
                  `(declare-nondeterministic ',function-name)))))))

(defun-compile-time modified-function-definitions (function-name environment)
  ;; NOTE: This is using the current rather than the saved ENVIRONMENT.
  (let ((function-record (get-function-record function-name))
        (callers (indirect-callers function-name))
        (function-records '()))
    (setf (function-record-old-deterministic? function-record)
          (function-record-deterministic? function-record))
    (setf (function-record-deterministic? function-record) t)
    (cached-push function-record function-records)
    (dolist (caller callers)
      (let ((function-record (get-function-record caller)))
        (unless (member function-record function-records :test #'eq)
          (setf (function-record-old-deterministic? function-record)
                (function-record-deterministic? function-record))
          (setf (function-record-deterministic? function-record) t)
          (cached-push function-record function-records))))
    (dolist (caller callers)
      (dolist (callee (callees caller))
        (let ((function-record (get-function-record callee)))
          (unless (member function-record function-records :test #'eq)
            (setf (function-record-old-deterministic? function-record)
                  (function-record-deterministic? function-record))
            (cached-push function-record function-records)))))
    (determine-whether-deterministic function-name environment)
    (determine-whether-callers-are-deterministic function-name nil environment)
    (let ((definitions (function-definition function-name environment)))
      (unless (eq (not (function-record-deterministic? function-record))
                  (not (function-record-old-deterministic? function-record)))
        (dolist (caller callers)
          (if (and (not (equal caller function-name))
                   (some #'(lambda (callee)
                             (let ((function-record (get-function-record callee)))
                               (not (eq (not (function-record-deterministic?
                                              function-record))
                                        (not (function-record-old-deterministic?
                                              function-record))))))
                         (callees caller)))
              (setf definitions
                    (append (function-definition caller environment)
                            definitions)))))
      ;; NOTE: This is so that macroexpand without compile doesn't get out of
      ;;       sync.
      (dolist (function-record function-records)
        (setf (function-record-deterministic? function-record)
              (function-record-old-deterministic? function-record)))
      ;; TODO: Test the below before uncommenting!
      ;; (release-list callers)
      ;; (release-list function-records)
      definitions)))

;;; The protocol

(defmacro-compile-time defun
    (function-name lambda-list &body body &environment environment)
  (let ((*compiling-nondeterministic-context?* t))
    (check-function-name function-name)
    (let* ((callees (compute-callees body environment))
           (function-record (get-function-record function-name))
           (function-record-lambda-list
             (function-record-lambda-list function-record))
           (function-record-body (function-record-body function-record))
           (function-record-callees (function-record-callees function-record))
           (function-record-deterministic?
             (function-record-deterministic? function-record))
           (function-record-old-deterministic?
             (function-record-old-deterministic? function-record))
           (function-record-screamer?
             (function-record-screamer? function-record)))
      (cache-definition function-name lambda-list body callees)
      (let ((modified-function-definitions
              ;; NOTE: This is using the current rather than the saved ENVIRONMENT.
              (modified-function-definitions function-name environment)))
        ;; NOTE: This is so that macroexpand without compile doesn't get out of
        ;;       sync.
        (setf (function-record-lambda-list function-record)
              function-record-lambda-list)
        (setf (function-record-body function-record) function-record-body)
        (setf (function-record-callees function-record)
              function-record-callees)
        (setf (function-record-deterministic? function-record)
              function-record-deterministic?)
        (setf (function-record-old-deterministic? function-record)
              function-record-old-deterministic?)
        (setf (function-record-screamer? function-record)
              function-record-screamer?)
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (cache-definition ',function-name ',lambda-list ',body ',callees)
           ,@modified-function-definitions
           ',function-name)))))

(defmacro-compile-time either (&body alternatives)
  "Nondeterministically evaluates and returns the value of one of its
ALTERNATIVES.

EITHER takes any number of arguments. With no arguments, \(EITHER) is
equivalent to \(FAIL) and is thus deterministic. With one argument, \(EITHER
X) is equivalent to X itself and is thus deterministic only when X is
deterministic. With two or more argument it is nondeterministic and can only
appear in a nondeterministic context.

It sets up a choice-point and evaluates the first ALTERNATIVE returning its
values. When backtracking follows to this choice-point, the next ALTERNATIVE
is evaluated and its values are returned. When no more ALTERNATIVES remain,
the current choice-point is removed and backtracking continues to the next
most recent choice-point."
  ;; FIXME: ref to operators providing nondeterministic contexts
  (cond ((not alternatives)
         '(fail))
        ((not (rest alternatives))
         (first alternatives))
        (t
         `(if (a-boolean)
              ,(first alternatives)
              (either ,@(rest alternatives))))))

(defmacro-compile-time either-prob-internal (&body alternatives)
  ;; FIXME: ref to operators providing nondeterministic contexts
  (cond ((not alternatives)
         '(fail))
        ((not (rest alternatives))
         (let* ((alt (first alternatives))
                (val (first alt))
                (prob (second alt)))
           (prog1
               `(progn
                  (trail-prob nil (* (current-probability)
                                     ,prob))
                  ,val)
             ;; TODO: Figure out why using `release-cons' here
             ;; leads to a stack overflow
             ;; (release-cons alternatives)
             )))
        (t
         `(if (a-boolean)
              (either-prob-internal ,(first alternatives))
              (either-prob-internal ,@(rest alternatives))))))

(defmacro-compile-time either-prob (&body alternatives)
  "Nondeterministically evaluates and returns the value of one of its
ALTERNATIVES, and updates the current probability accordingly.

Acts as EITHER, but alternatives are 2-element lists
of values and probabilities.

Probabilities must be provided as numbers, which will be normalized
to probabilities between 0 and 1 (i.e. (either-prob (1 2) (2 1/2))
would give 4/5 chance to 1 and 1/5 chance to 2).

Probabilities cannot be provided as forms that evaluate
to numbers.

If any choices are provided without associated probabilities
(i.e. not a 2-element list with the second element a number),
then they will be assigned the average probability number of
those elements where such was provided. If no elements
match this pattern, then the uniform prior will be used.

If a choice is a 2-element list with the second element nil,
it will be treated as if the first element was the value
and no probability was provided (i.e. (either-prob ((+ 1) nil) 2)
gives equal probability to 1 and 2)."
  (flet ((normalize (alt-list)
           (let* ((prob-pred (andf #'listp
                                   (compose #'numberp
                                            #'second)
                                   (compose (curry #'= 2)
                                            #'length)))
                  (prob-provided (s:~>> alt-list
                                        ;; Filter by valid probability values
                                        (s:filter prob-pred)
                                        ;; Extract probability values
                                        (mapcar #'second)))
                  (prob-provided (or prob-provided (list 1)))
                  (prob-sum (apply #'+ prob-provided))
                  (prob-avg (/ prob-sum (length prob-provided)))
                  (prob-sum (* prob-avg (length alt-list)))
                  (prob-ignore-pred (andf #'listp
                                          (compose #'null
                                                   #'second)
                                          (compose (curry #'= 2)
                                                   #'length)))
                  (alt-list (mapcar (lambda (elem)
                                      (if (funcall prob-ignore-pred elem)
                                          (list (first elem)
                                                prob-avg)
                                          elem))
                                    alt-list))
                  (normalized (mapcar (lambda (elem)
                                        (if (funcall prob-pred elem)
                                            (list (first elem)
                                                  (/ (second elem)
                                                     prob-sum))
                                            (list elem
                                                  (/ prob-avg
                                                     prob-sum))))
                                      alt-list)))
             (list prob-provided)
             (list alt-list)
             normalized)))
    `(either-prob-internal
       ,@(sort (normalize alternatives) #'> :key #'second))))

(defmacro-compile-time local (&body body &environment environment)
  "Evaluates BODY in the same fashion as PROGN except that all SETF and SETQ
forms lexically nested in its body result in local side effects which are
undone upon backtracking.

This affects only side effects introduced explicitly via SETF and SETQ. Side
effects introduced by either user defined functions or builtin Common Lisp
functions such as RPLACA are always global.

Behaviour of side effects introduced by macro-expansions such as INCF depends
on the exact macro-expansion. If (INCF (FOO)) expands using eg. SET-FOO, LOCAL
is unable to undo the side-effect.

LOCAL cannot distinguish between initially uninitialized and intialized
places, such as unbound variables or hash-table keys with no prior values. As
a result, an attempt to assign an unbound variable inside LOCAL will signal an
error due to the system's attempt to first read the variable. Similarly,
undoing a (SETF GETHASH) when the key did not previously exist in the table
will insert a NIL into the table instead of doing a REMHASH. Easiest way
to work around this is by using TRAIL.

LOCAL and GLOBAL may be nested inside one another. The nearest lexically
surrounding one determines whether or not a given SETF or SETQ results in a
local or global side effect.

Side effects default to be global when there is no surrounding LOCAL or GLOBAL
expression. Local side effects can appear both in deterministic as well as
nondeterministic contexts though different techniques are used to implement
the trailing of prior values for restoration upon backtracking. In
nondeterministic contexts, LOCAL as well as SETF are treated as special forms
rather than macros. This should be completely transparent to the user."
  (let ((*local?* t))
    `(progn ,@(mapcar
               #'(lambda (form) (perform-substitutions form environment))
               body))))

;;; TODO: Figure out how to make LOOP work with global
(defmacro-compile-time global (&body body &environment environment)
  "Evaluates BODY in the same fashion as PROGN except that all SETF and SETQ
forms lexically nested in its body result in global side effects which are not
undone upon backtracking.

Note that this affects only side effects introduced explicitly via SETF and
SETQ. Side effects introduced by Common Lisp builtin functions such as RPLACA
are always global anyway.

LOCAL and GLOBAL may be nested inside one another. The nearest lexically
surrounding one determines whether or not a given SETF or SETQ results in a
local or global side effect.

Side effects default to be global when there is no surrounding LOCAL or GLOBAL
expression. Global side effects can appear both in deterministic as well as
nondeterministic contexts. In nondeterministic contexts, GLOBAL as well as
SETF are treated as special forms rather than macros. This should be
completely transparent to the user."
  (let ((*local?* nil))
    `(progn ,@(mapcar
               #'(lambda (form) (perform-substitutions form environment))
               body))))

(defmacro-compile-time for-effects (&body body &environment environment)
  "Evaluates BODY as an implicit PROGN in a nondeterministic context and
returns NIL.

The body is repeatedly backtracked to its first choice-point until the body
fails.

Local side effects performed by BODY are undone when FOR-EFFECTS returns.

A FOR-EFFECTS expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the FOR-EFFECTS appears in, BODY are
always in a nondeterministic context. A FOR-EFFECTS expression is is always
deterministic."
  `(prog1
       (catch '%escape
         (let ((*screamer-failures* 0))
           (choice-point
            ,(let ((*compiling-nondeterministic-context?* t))
               (cps-convert-progn body '#'fail nil nil environment)))))
     (unless *nondeterministic-context*
       (setf *pure-cache* nil))))


(defvar *last-value-cons* nil
  "Internal variable tracking the end of `*screamer-results*'
for purposes of result collection.

Used in other methods (like `call/cc') to maintain stable
backtracking/collection with nonstandard control flows.")

(defun copy-output-value (value)
  (etypecase value
    (cons (copy-tree value))
    (hash-table (let ((ret (copy-hash-table value)))
                  (maphash (lambda (k v) (setf (gethash k ret) (copy-output-value v))) ret)
                  ret))
    (sequence (let ((ret (copy-seq value)))
                (dotimes (idx (length ret))
                  (setf (elt ret idx)
                        (copy-output-value (elt ret idx))))
                ret))
    (structure-object (copy-structure value))
    (t value)))

(defmacro-compile-time all-values (&body body)
  "Evaluates BODY as an implicit PROGN and returns a list of all of the
nondeterministic values yielded by the it.

These values are produced by repeatedly evaluating the body and backtracking
to produce the next value, until the body fails and yields no further values.

Accordingly, local side effects performed by the body while producing each
value are undone before attempting to produce subsequent values, and all local
side effects performed by the body are undone upon exit from ALL-VALUES.

Returns a list containing NIL if BODY is empty.

An ALL-VALUES expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the ALL-VALUES appears in, the BODY is
always in a nondeterministic context. An ALL-VALUES expression itself is
always deterministic.

ALL-VALUES is analogous to the `bagof' primitive in Prolog."
  (let ((values '*screamer-results*)
        ;; TODO: Figure out why we can't make this a `gensym'
        (value 'value))
    `(let ((,values nil)
           (*last-value-cons* nil))
       (for-effects
         (local (setf (gethash :screamer-accumulation-strategy *nondeterministic-context*) (list :all-values)))
         (let* ((,value (progn ,@body))
                (,value (copy-output-value ,value)))
           (global (if (null ,values)
                       (setf *last-value-cons* (list ,value)
                             ,values *last-value-cons*)
                       (setf (rest *last-value-cons*) (list ,value)
                             *last-value-cons* (rest *last-value-cons*))))))
       (if *possibility-consolidator*
           (flet ((merge-vals (vals)
                    (let ((prev nil))
                      (mapc (lambda (v)
                              (unless (position v prev
                                                :test *possibility-consolidator*)
                                (push v prev)))
                            vals)
                      (list vals)
                      prev)))
             (merge-vals ,values))
           ,values))))

(defmacro-compile-time all-values-prob (&body body)
  "Evaluates BODY as an implicit PROGN and returns a list pairing all of the
nondeterministic values yielded by it with their corresponding probabilities.

Other than the output format, semantics are equivalent to
ALL-VALUES.

Note that some possibilities may have duplicate return values.

Note that the probabilities are measured with respect to the
distributions provided at probabilistic choice points; if
constraints or FAIL calls remove potential branches, then the
sum of the probabilities returned will be less than 1."
  (let ((values '*screamer-results*)
        (pointer (gensym "enclosing-trail-pointer"))
        ;; TODO: Figure out why we can't make this a `gensym'
        (value 'value))
    (alexandria:with-gensyms (v merge-vals prev prev-val)
      `(let ((,values '())
             (*last-value-cons* nil)
             ;; Reset probability
             (,pointer (prog1 (fill-pointer *trail*)
                         (trail-prob nil 1))))
         ;; Process BODY
         (for-effects
           (local (setf (gethash :screamer-accumulation-strategy *nondeterministic-context*) (list :all-values)))
           (let* ((,value (progn ,@body))
                  (,value (copy-output-value ,value)))
             (global (if (null ,values)
                         (setf *last-value-cons* (list
                                                  (list ,value
                                                        (current-probability *trail*)))
                               ,values *last-value-cons*)
                         (setf (rest *last-value-cons*) (list
                                                         (list ,value
                                                               (current-probability *trail*)))
                               *last-value-cons* (rest *last-value-cons*))))))
         ;; Return to enclosing trail context
         (unwind-trail-to ,pointer)
         ;; Consolidate probabilities
         (if *possibility-consolidator*
             (flet ((,merge-vals (vals)
                      (let ((,prev nil))
                        (mapc (lambda (,v)
                                (if-let (,prev-val (assoc (first ,v) ,prev
                                                         :test *possibility-consolidator*))
                                  (incf (second ,prev-val)
                                               (second ,v))
                                  (push ,v ,prev)))
                              vals)
                        ,prev)))
               (declare (inline ,merge-vals))
               (,merge-vals ,values))
             ,values)))))

(defmacro-compile-time expected-prob (&body body)
  "Returns the sum of the probabilities of all the values produced by
ALL-VALUES-PROB.

If there are no non-probabilistic branches in BODY, then this corresponds
to the total probability of the constraints in BODY being satisfied."
  `(reduce (lambda (a b) (+ a (second b)))
    (all-values-prob
      ,@body)
    :initial-value 0))

(defmacro-compile-time expected-value (&body body)
  "Returns the sum of each value produced by body times its probability,
divided by the sum of the probabilities.

If there are no non-probabilistic branches in BODY, then this corresponds
to the expected value of the output of BODY.

Throws an exception if body outputs any non-numeric value.

Returns NIL if all branches of BODY fail."
  (with-gensyms (aggregates)
    `(let ((,aggregates
             (funcall (s:juxt (s:op (reduce #'+ _ :key (curry #'reduce #'*)))
                              (s:op (reduce #'+ _ :key #'second)))
                      (all-values-prob ,@body))))
       (prog1
           (unless (zerop (second ,aggregates))
             (/ (first ,aggregates) (second ,aggregates)))
         ;; Release the juxt list to the cons cache
         (list ,aggregates))))
  ;; NOTE: Old version of this code
  ;; `(s:nest
  ;;   (funcall (lambda (x)
  ;;              (prog1
  ;;                  (unless (zerop (second x))
  ;;                    (/ (first x) (second x)))
  ;;                ;; Release the juxt list to the cons cache
  ;;                (list x))))
  ;;   (funcall (s:juxt (lambda (x) (reduce #'+ x :key (curry #'reduce #'*)))
  ;;                    (lambda (x) (reduce #'+ x :key #'second))))
  ;;   (all-values-prob ,@body))
  )

(defmacro-compile-time n-values ((n &key (default nil default-on-failure)) &body body)
  "Returns the first N nondeterministic values yielded by BODY.

N must be an integer denoting the number of values to return, or a form
producing such an integer.

No further execution of BODY is attempted after it successfully yields the
desired value.

If BODY fails before yielding the N values to be returned, then if DEFAULT is
provided it is evaluated and its value returned instead. If DEFAULT is not
specified, the values that were produced so far are returned.
Note that DEFAULT can be specified as `nil'.

Local side effects performed by BODY are undone when N-VALUES returns, but
side effects performed by DEFAULT and N defer to the context outside N-VALUES
for this behavior.

An N-VALUES expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the N-VALUES appears in, BODY is
always in a nondeterministic context, while DEFAULT and N are in whatever
context the N-VALUES appears in.

An N-VALUES expression is nondeterministic if DEFAULT is present and is
nondeterministic, or if N is nondeterministic. Otherwise it is deterministic.

If DEFAULT is present and nondeterministic, and if BODY fails, then it is
possible to backtrack into the DEFAULT and for the N-VALUES expression to
nondeterministically return multiple times.

If N is nondeterministic then the N-VALUES expression operates
nondeterministically on each value of N. In this case, backtracking for each
value of BODY and DEFAULT is nested in, and restarted for, each backtrack of
N."
  (when (numberp n) (assert (typep n '(integer 0))))
  (let* ((acc-strat `(gethash :screamer-accumulation-strategy *nondeterministic-context*))
         ;; TODO: Figure out why we can't make this a `gensym'
         (value 'value)
         (value-list '*screamer-results*))
    `(block n-values
       (let* ((,value-list nil)
              (*last-value-cons* nil))
         (declare ((or cons null) ,value-list))
         (or
          (for-effects
            ;; Store the accumulation strategy
            (local (setf ,acc-strat (list :n-values (value-of ,n))))
            (unless (zerop (second ,acc-strat))
              (global
                (let* ((,value (progn ,@body))
                       (,value (copy-output-value ,value)))

                  ;; Add the value to the collected list
                  (appendf ,value-list (list ,value))
                  ;; (if (null ,value-list)
                  ;;     (setf *last-value-cons* (list ,value)
                  ;;           ,value-list *last-value-cons*)
                  ;;     (setf (rest *last-value-cons*) (list ,value)
                  ;;           *last-value-cons* (rest *last-value-cons*)))
                  (when (>= (length ,value-list) (second ,acc-strat))
                    ;; (return-from n-values ,value-list)
                    (escape ,value-list))))))
          ,(if default-on-failure default value-list))))))

(defmacro-compile-time n-values-prob ((n &key (default nil default-on-failure)) &body body)
  "Identical to N-VALUES, but returns pairs of values and probabilities.
See the docstring of `ALL-VALUES-PROB' for more details."
  (when (numberp n) (assert (typep n '(integer 0))))
  (let ((acc-strat `(gethash :screamer-accumulation-strategy *nondeterministic-context*))
        ;; TODO: Figure out why we can't make this a `gensym'
        (value 'value)
        (value-list '*screamer-results*)
        (pointer (gensym "enclosing-trail-pointer")))
    `(block n-values
       (let ((,value-list nil)
             (*last-value-cons* nil)
             ;; Reset probability
             (,pointer (prog1 (fill-pointer *trail*)
                         (trail-prob nil 1))))
         (declare ((or cons null) ,value-list))
         ;; Process BODY
         (prog1
             (or
              (for-effects
                (local (setf ,acc-strat (list :n-values (value-of ,n))))
                (unless (zerop (second ,acc-strat))
                  (let* ((,value (progn ,@body))
                         (,value (copy-output-value ,value))
                         (,value (list ,value
                                       (current-probability *trail*))))
                    ;; Add the value to the collected list
                    (appendf ,value-list (list ,value))
                    ;; (if (null ,value-list)
                    ;;     (setf *last-value-cons* (list ,value)
                    ;;           ,value-list *last-value-cons*)
                    ;;     (setf (rest *last-value-cons*) (list ,value)
                    ;;           *last-value-cons* (rest *last-value-cons*)))
                    (when (>= (length ,value-list) (second ,acc-strat))
                      ;; Return to enclosing trail context
                      (unwind-trail-to ,pointer)
                      (escape ,value-list)))))
              ,(if default-on-failure default value-list))
           ;; Return to enclosing trail context
           (unwind-trail-to ,pointer))))))

(defmacro-compile-time kth-value ((k &key (default '(fail))) &body body)
  "Returns the Kth nondeterministic value yielded by BODY.

K must be an integer. The first nondeterministic value yielded by BODY is
numbered zero, the second one, etc. The Kth value is produced by repeatedly
evaluating BODY, backtracking through and discarding the first K values and
deterministically returning the next value produced.

No further execution of BODY is attempted after it successfully yields the
desired value.

If BODY fails before yielding both the I values to be discarded, as well as
the desired Kth value, then DEFAULT is evaluated and its value returned
instead. DEFAULT defaults to \(FAIL) if not present.

Local side effects performed by BODY are undone when KTH-VALUE returns, but
local side effects performed by DEFAULT and by I are not undone when KTH-VALUE
returns.

An KTH-VALUE expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the KTH-VALUE appears in, BODY is
always in a nondeterministic context, while DEFAULT and I are in whatever
context the KTH-VALUE appears in.

An KTH-VALUE expression is nondeterministic if DEFAULT is present and is
nondeterministic, or if I is nondeterministic. Otherwise it is deterministic.

If DEFAULT is present and nondeterministic, and if BODY fails, then it is
possible to backtrack into the DEFAULT and for the KTH-VALUE expression to
nondeterministically return multiple times.

If I is nondeterministic then the KTH-VALUE expression operates
nondeterministically on each value of I. In this case, backtracking for each
value of BODY and DEFAULT is nested in, and restarted for, each backtrack of
I."
  `(car
    (last
     (n-values ((1+ ,k) :default (list ,default))
       ,@body))))

(defmacro-compile-time ith-value (i form &optional (default '(fail)))
  "DEPRECATED
This form is retained for compatibility with classic Screamer. Please use
`KTH-VALUE' instead."
  (let ((counter (gensym "I")))
    `(block ith-value
       (let ((,counter (value-of ,i)))
         (for-effects (let ((value ,form))
                        (if (zerop ,counter)
                            (return-from ith-value value)
                            (decf ,counter))))
         ,default))))

(defmacro-compile-time one-value (form &optional (default '(fail)))
  "Returns the first nondeterministic value yielded by FORM.

No further execution of FORM is attempted after it successfully returns one
value.

If FORM does not yield any nondeterministic values \(i.e. it fails) then
DEFAULT is evaluated and its value returned instead. DEFAULT defaults to
\(FAIL) if not present.

Local side effects performed by FORM are undone when ONE-VALUE returns, but
local side effects performed by DEFAULT are not undone when ONE-VALUE returns.

A ONE-VALUE expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the ONE-VALUE appears in, FORM is
always in a nondeterministic context, while DEFAULT is in whatever context the
ONE-VALUE form appears.

A ONE-VALUE expression is nondeterministic if DEFAULT is present and is
nondeterministic, otherwise it is deterministic.

If DEFAULT is present and nondeterministic, and if FORM fails, then it is
possible to backtrack into the DEFAULT and for the ONE-VALUE form to
nondeterministically return multiple times. ONE-VALUE is analogous to the cut
primitive \(`!') in Prolog."
  `(kth-value (0 :default ,default) ,form))

(defmacro-compile-time possibly? (&body body)
  "Evaluates BODY as an implicit PROGN in nondeterministic context,
returning true if the body ever yields true.

The body is repeatedly backtracked as long as it yields NIL. Returns
the first true value yielded by the body, or NIL if body fails before
yielding true.

Local side effects performed by the body are undone when POSSIBLY? returns.

A POSSIBLY? expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the POSSIBLY? appears in, its body is
always in a nondeterministic context. A POSSIBLY? expression is always
deterministic."
  `(one-value (let ((value (progn ,@body))) (unless value (fail)) value) nil))

(defmacro-compile-time necessarily? (&body body)
  "Evaluates BODY as an implicit PROGN in nondeterministic context,
returning true if the body never yields false.

The body is repeatedly backtracked as long as it yields true. Returns the last
true value yielded by the body if it fails before yielding NIL, otherwise
returns NIL.

Local side effects performed by the body are undone when NECESSARILY? returns.

A NECESSARILY? expression can appear in both deterministic and
nondeterministic contexts. Irrespective of what context the NECESSARILY?
appears in, its body is always in a nondeterministic context. A NECESSARILY?
expression is always deterministic."
  `(let ((result t))
     (one-value
         (let ((value (progn ,@body)))
           (when value (setf result value) (fail))
           value)
         result)))

(defmacro-compile-time uniquely? (&body body)
  "Evaluates BODY as an implicit PROGN in nondeterministic context,
returning true if the body has exactly one possible value.

The body is repeatedly backtracked as long as it has only returned up to one
value or until it fails and returns no values, whichever comes first.

Returns the unique value yielded by the body if such a value exists, otherwise
returns NIL.

Local side effects performed by the body are undone when UNIQUELY? returns.

A UNIQUELY? expression can appear in both deterministic and
nondeterministic contexts. Irrespective of what context the UNIQUELY?
appears in, its body is always in a nondeterministic context. A UNIQUELY?
expression is always deterministic."
  (with-gensyms (result)
    `(let ((,result (n-values (2) ,@body)))
       (when (= 1 (length ,result))
         (car ,result)))))

;;; Memoization
(defvar-compile-time *pure-cache* nil
  "An alist storing cached location-keys and
corresponding cache hashmaps for `screamer:pure'
forms")

;; TODO: Remove prints in the below
(cl:defun cache-pure-ensure (location-key)
  ;; (print "ensure")
  (cdr (or (assoc location-key *pure-cache* :test 'equal)
           (progn
             (push (cons location-key (s:dict))
                   *pure-cache*)
             (first *pure-cache*)))))

(cl:defun cache-pure-retrieve (location-key parameters)
  ;; (print "retrieve")
  (let* ((cache (cache-pure-ensure location-key)))
    ;; (format t "~%pre-retrieve cache: ~A" cache)
    ;; (print "hel")
    ;; (format t "retrieving loc ~A params ~A. output ~A"
    ;;         location-key
    ;;         parameters
    ;;         cache)
    (gethash parameters cache)))

(cl:defun cache-pure-put (location-key parameters value)
  (let* ((cache (cache-pure-ensure location-key))
         (value (etypecase value
                  (list (copy-tree value))
                  (vector (copy-array value))
                  (t value))))
    ;; (format t "~%putting loc ~A params ~A value ~A into ~A"
    ;;         location-key
    ;;         parameters
    ;;         value
    ;;         cache)
    (setf (gethash parameters cache) value)
    ;; (format t "~%post-put cache: ~A" cache)
    value))

;;; TODO: Finish and validate the PURE functionality
;;; TODO: Figure out if the name should stay PURE or change to something
;;; else.
;;; MEMO maybe? Seems like that would conflict with actual memoization
;;; packages though...
;;; SCREAMER-MEMO or SCREAMER-PURE are verbose, but at least not conflicting?
;;;
;; FIXME: Seems to be some bug in pure-values?
(serapeum:example
  ;; Test case
  (all-values (fib-cached (a-member-of '(0 1 2 3 4 5 6 7 8))))
  ;; Works
  (screamer::defun fib-cached (idx)
    (screamer::pure-one-value (idx)
      (or
       (case idx (0 0) (1 1) (2 1))
       (+ (fib-cached (1- idx)) (fib-cached (- idx 2))))))
  ;; Does bad things
  (screamer::defun fib-cached (idx)
    (screamer::pure-values (idx)
      (or
       (case idx (0 0) (1 1) (2 1))
       (+ (fib-cached (1- idx)) (fib-cached (- idx 2)))))))
(defmacro-compile-time pure-values (parameters &body body &environment environment)
  "EXPERIMENTAL
Evaluates BODY as an implicit LOCAL form. PARAMETERS is a list of expressions
whose outputs are treated as unique keys for this form; if a set of parameters has
been previously encountered within the current instance of nondeterministic context,
the output from that execution will be retrieved from cache without evaluating BODY.

Note that all possible values of BODY are iterated through before PURE-VALUES starts
emitting return values.

Note that this is not a probabilistic form, and will not propagate probabilities in
BODY to the containing nondeterministic context."
  (declare (ignore environment))
  (let ((loc-key (gensym "screamer-pure-values")))
    (with-gensyms (param-outputs cache-value)
      `(global
         (let* ((,param-outputs (list ,@parameters))
                (,cache-value (screamer::cache-pure-retrieve ',loc-key ,param-outputs)))
           (a-member-of
            (or ,cache-value
                (screamer::cache-pure-put ',loc-key
                                          ,param-outputs
                                          (all-values
                                            (local
                                              ,@body))))))))))

(defmacro-compile-time pure-one-value (parameters &body body &environment environment)
  "EXPERIMENTAL
Like PURE-VALUES, but only caches/outputs one value from the body, similar to ONE-VALUE"
  (declare (ignore environment))
  (let ((loc-key (gensym "screamer-pure-one-value")))
    (with-gensyms (param-outputs cache-value)
      `(global
         (let* ((,param-outputs (list ,@parameters))
                (,cache-value (screamer::cache-pure-retrieve ',loc-key ,param-outputs)))
           (or ,cache-value
               (screamer::cache-pure-put ',loc-key
                                         ,param-outputs
                                         (one-value (local ,@body)))))))))


;;; In classic Screamer TRAIL is unexported and UNWIND-TRAIL is exported. This
;;; doesn't seem very safe or sane: while users could conceivably want to use
;;; TRAIL to track unwinds, using UNWIND-TRAIL seems inherently dangerous
;;; given that Screamer uses TRAIL internally.
;;;
;;; So, we export TRAIL, and document UNWIND-TRAIL as being deprecated,
;;; and plan to delete it before 4.0.
(declaim (inline trail trail-prob))
(defun trail (obj)
  "When called in non-deterministic context, adds OBJ to the trail.
Outside non-deterministic context does nothing.

Functions on the trail are called when unwinding from a nondeterministic
selection (due to either a normal return, or calling FAIL.)"
  ;; NOTE: Is it really better to use VECTOR-PUSH-EXTEND than CONS for the
  ;;       trail?
  (declare (optimize (speed 3) (space 3) (debug 0)))
  (when *nondeterministic-context*
    (vector-push-extend obj *trail* 1024))
  obj)
(defun trail-prob (obj prob)
  (declare (number prob)
           (optimize (speed 3) (space 3) (debug 0)))
  (when *nondeterministic-context*
    (vector-push-extend
     (cond ((and obj prob) (list obj prob))
           (obj obj)
           (prob prob))
     *trail*
     1024)))

(declaim (inline pop-trail))
(defun pop-trail (trail)
  (vector-pop trail))

(defun unwind-trail-to (trail-pointer)
  (declare (fixnum trail-pointer))
  ;; KLUDGE: This loops needs to be kept simple, since in some implementations
  ;; (eg. Lispworks) non-trivial LOOP generates MACROLETs that can't be
  ;; supported by WALK.
  (let ((trail *trail*))
    (loop (when (<= (fill-pointer trail) trail-pointer)
            (return-from unwind-trail-to))
          (let ((fun (pop-trail trail)))
            (when (consp fun)
              ;; Complex trail elements always have a function or nil as their first element
              (setf fun (car fun)))
            (when (functionp fun)
              (funcall fun)))
          ;; NOTE: This is to allow the trail closures to be garbage collected.
          (setf (aref trail (fill-pointer trail)) nil))))

;;; FIXME: Since Screamer doesn't use UNWIND-TRAIL even internally, it should
;;; probably be deleted when Screamer 4.0 is in the works.
(defun unwind-trail ()
  "DEPRECATED.

Calls all functions installed using TRAIL, and removes them from the trail.

Using UNWIND-TRAIL is dangerous, as TRAIL is used by Screamer internally to
eg. undo effects of local assignments -- hence users should never call it. It
is provided at the moment only for backwards compatibility with classic
Screamer."
  (unwind-trail-to 0))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'collect-trail))
(cl:defun collect-trail ()
  "EXPERIMENTAL
Pushes a copy of Screamer's `*trail*' to the `:trail' key of `*nondeterministic-context*'.

Note that this function ignores `local' and `global'; the list of copies will continue
growing until you exit nondeterministic context.

Functions which process `*trail*' (e.g. `current-probability') can be used to
extract information from the collected trails."
  (screamer-error
   "COLLECT-TRAIL is a nondeterministic function. As such, it must be called only~%~
   from a nondeterministic context."))
(cl:defun collect-trail-nondeterministic (continuation)
  (push (copy-seq *trail*) (gethash :trail *nondeterministic-context*))
  (funcall continuation t))


(defun current-probability (&optional (trail *trail*))
  (declare (optimize (speed 3)))
  (labels ((zero-one (n)
             (typecase n
               ;; N is a number between 0 and 1
               (number (<= 0 n 1))))
           (get-trail-prob (elem)
             (cond ((zero-one elem) elem)
                   ((and (listp elem)
                         (zero-one (second elem)))
                    (second elem)))))
    (the number
         (or (find-if #'identity trail :from-end t :key #'get-trail-prob) 1))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'factor-prob))
(cl:defun factor-prob (p)
  "Multiply the current Screamer probability by P for the rest of this nondeterministic path.
The current probability is determined by `current-probability'.

Returns the new probability.

Example:
(all-values-prob (if (a-boolean-prob 3/4) (progn (factor-prob (/ 1 (current-probability))) (a-boolean-prob 1/2)) (fail)))
=> ((T 1/2) (NIL 1/2))"
  (declare (ignore p) (number p))
  (screamer-error
   "FACTOR-PROB is a nondeterministic function. As such, it must be called only~%~
   from a nondeterministic context."))
(cl:defun factor-prob-nondeterministic (continuation p)
  (declare (function continuation) (number p)
           (optimize (speed 3) (space 3) (debug 1)))
  (choice-point-external
   (trail-prob nil (* (current-probability) p))
   (choice-point-internal
    (funcall continuation (current-probability)))))

(cl:defun normalize-probabilities (result-list &key (total nil)
                                   &aux (total (or total
                                                   (reduce #'+
                                                           ;; Filter out malformed results (i.e. not
                                                           ;; a probability-value pair)
                                                           (remove-if-not #'listp result-list)
                                                           :key #'second))))
  "Normalize a list of lists with form (_ PROBABILITY &rest _).

TOTAL is a number representing the total probability mass. This will be
treated as 1 post-normalization, with the individual probabilities multiplied
by the same factor.
If not provided, it defaults to the sum of the probabilities.

Example:
(normalize-probabilities
    (all-values-prob
        (if (a-boolean-prob 3/4)
            (a-boolean-prob 1/2)
          (progn (factor-prob 2)
                 (a-boolean-prob 1/2)))))

=> ((T 3/10) (NIL 3/10) (T 1/5) (NIL 1/5))"
  (declare (list result-list) (type (or number null) total)
           (optimize (speed 3)))
  (mapcar (lambda (result)
            ;; Generate a copy of each result to avoid
            ;; mutating the input
            (let ((new-result (copy-list result)))
              ;; Divide the probabilities by their sum to normalize
              (s:callf (rcurry #'/ total)
                       (second new-result))
              new-result))
          result-list))

(defun y-or-n-p
    (&optional (format-string nil format-string?) &rest format-args)
  (cond
    (*iscream?*
     (let ((query (if format-string?
                      (format nil "~A (Y or N): "
                              (apply #'format nil format-string format-args))
                      "(Y or N): ")))
       (emacs-eval '(y-or-n-p-begin))
       (unwind-protect
            (tagbody
             loop
               (format *query-io* "~%~A" query)
               (let ((char (read-char *query-io*)))
                 (when (or (char= char #\y) (char= char #\Y))
                   (format *query-io* "Y")
                   (return-from y-or-n-p t))
                 (when (or (char= char #\n) (char= char #\N))
                   (format *query-io* "N")
                   (return-from y-or-n-p nil)))
               (format *query-io* "Please type a single character, Y or N")
               (go loop))
         (emacs-eval '(y-or-n-p-end)))))
    (format-string? (apply #'cl:y-or-n-p format-string format-args))
    (t (cl:y-or-n-p))))

(defmacro-compile-time print-values (&body body)
  "Evaluates BODY as an implicit PROGN and prints each of the nondeterministic
values yielded by it using PRINT.

After each value is printed, the user is queried as to whether or not further
values are desired. These values are produced by repeatedly evaluating the
body and backtracking to produce the next value, until either the user
indicates that no further values are desired or until the body fails and
yields no further values.

Returns the last value printed.

Accordingly, local side effects performed by the body while producing each
value are undone after printing each value, before attempting to produce
subsequent values, and all local side effects performed by the body are undone
upon exit from PRINT-VALUES, either because there are no further values or
because the user declines to produce further values.

A PRINT-VALUES expression can appear in both deterministic and
nondeterministic contexts. Irrespective of what context the PRINT-VALUES
appears in, the BODY are always in a nondeterministic context. A
PRINT-VALUES expression itself is always deterministic.

PRINT-VALUES is analogous to the standard top-level user interface in Prolog."
  `(catch 'succeed
     (for-effects
       (let ((value (progn ,@body)))
         (print value)
         (unless (y-or-n-p "Do you want another solution?")
           (throw 'succeed value))))))

;;; NOTE: Should have way of having a stream of values.

(eval-when (:compile-toplevel :load-toplevel :execute) (setf *screamer?* t))

(defun print-nondeterministic-function
    (nondeterministic-function stream print-level)
  (declare (ignore print-level))
  (format stream "#<~A ~S>"
          'nondeterministic
          (nondeterministic-function-function nondeterministic-function)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'a-boolean))

(cl:defun a-boolean ()
  "Equivalent to \(EITHER T NIL)."
  (screamer-error
   "A-BOOLEAN is a nondeterministic function. As such, it must be called only~%~
   from a nondeterministic context."))

(cl:defun a-boolean-nondeterministic (continuation)
  (choice-point (funcall continuation t))
  (funcall continuation nil))

(defun a-boolean-prob (&optional (p 1/2))
  "Equivalent to \(EITHER-PROB (T p) (NIL (- 1 p))."
  (assert (<= 0 p 1) nil "`a-boolean-prob' was given ~A, not a valid probability!" p)
  (if (>= p 1/2)
      (either-prob-internal (t p) (nil (- 1 p)))
      (either-prob-internal (nil (- 1 p)) (t p))))

(defvar-compile-time *escape*
    (lambda (result)
      (throw '%escape result)))

(defun-compile-time escape (result)
  "Throws an `%escape' tag. Used to ensure
forms like `with-failing' also affect
escapes."
  (funcall *escape* result))

(defmacro-compile-time when-escaping ((&body escaping-forms) &body body)
  "Whenever ESCAPE is called during execution of BODY, executes ESCAPING-FORMS
before unwinding."
  (let ((old-escape (gensym "ESCAPE")))
    `(let* ((,old-escape *escape*)
            (*escape* (lambda (result) ,@escaping-forms (funcall ,old-escape result))))
       ,@body)))

(defvar-compile-time *fail*
    (lambda ()
      (when *screamer-max-failures*
        (incf *screamer-failures*)
        (when (> *screamer-failures* *screamer-max-failures*)
          (escape nil)))
      (if *nondeterministic-context*
          (throw '%fail nil)
          (error "Cannot FAIL: no choice-point to backtrack to."))))

(defun-compile-time fail ()
  "Backtracks to the most recent choice-point.

FAIL is deterministic function and thus it is permissible to reference #'FAIL,
and write \(FUNCALL #'FAIL) or \(APPLY #'FAIL).

Calling FAIL when there is no choice-point to backtrack to signals an error."
  (funcall *fail*))

(defmacro-compile-time when-failing ((&body failing-forms) &body body)
  "Whenever FAIL is called during execution of BODY, executes FAILING-FORMS
before unwinding.

Note that this does not catch invocations of ESCAPE. For those, use
WHEN-ESCAPING."
  (let ((old-fail (gensym "FAIL")))
    `(let* ((,old-fail *fail*)
            (*fail* (lambda () ,@failing-forms (funcall ,old-fail))))
       ,@body)))

(defmacro-compile-time count-failures (&body body)
  "Executes BODY keeping track of the number of times FAIL has been called
without unwinding from BODY. After BODY completes, reports the number of
failures to *STANDARD-OUTPUT* before returning values from BODY."
  (let ((values (gensym "VALUES-")))
    `(let ((failure-count 0)) ; FIXME: use a gensym after 3.21 -- now backwards compat is king
       (when-failing ((incf failure-count))
         (let ((,values (multiple-value-list (progn ,@body))))
           (format t "Failures         = ~10<~;~d~>" failure-count)
           (values-list ,values))))))

(defun nondeterministic-function? (x)
  "Returns T if X is a nondeterministic function and NIL otherwise.

#'FOO returns a nondeterministic function object iff it is used in nondeterminisitc
context and FOO is either a nondeterministic LAMBDA form, or the name of a
nondeterministic function defined using SCREAMER::DEFUN.

Currently, if FOO is a nondeterministic function defined using
SCREAMER::DEFUN, #'FOO and \(SYMBOL-FUNCTION 'FOO) in deterministic context
will return an ordinary deterministic Common Lisp function, which will signal
an error at runtime."
  (nondeterministic-function?-internal (value-of x)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'funcall-nondeterministic))

(cl:defun funcall-nondeterministic (function &rest arguments)
  "Analogous to CL:FUNCALL, except FUNCTION can be either a nondeterministic
function, or an ordinary determinisitic function.

You must use FUNCALL-NONDETERMINISTIC to funcall a nondeterministic function.
An error is signalled if you attempt to funcall a nondeterministic
function object with CL:FUNCALL.

You can use FUNCALL-NONDETERMINISTIC to funcall either a deterministic or
nondeterministic function, though even if all of the ARGUMENTS are
deterministic and FUNCTION is a deterministic function object, the call
expression will still be nondeterministic \(with presumably a single value),
since it is impossible to determine at compile time that a given call to
FUNCALL-NONDETERMINISTIC will be passed only deterministic function objects
for function."
  (declare (ignore function arguments))
  (screamer-error
   "FUNCALL-NONDETERMINISTIC is a nondeterministic function. As such, it~%~
   must be called only from a nondeterministic context."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline funcall-nondeterministic-nondeterministic)))
(cl:defun funcall-nondeterministic-nondeterministic
    (continuation function &rest arguments)
  (declare (optimize (speed 3) (space 3))
           (function continuation)
           (type (or function nondeterministic-function variable null) function))
  (let ((function (value-of function)))
    (if (nondeterministic-function? function)
        (apply (nondeterministic-function-function function)
               continuation
               arguments)
        (funcall continuation (apply function arguments)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'apply-nondeterministic))

(cl:defun apply-nondeterministic (function &rest arguments)
  "Analogous to the CL:APPLY, except FUNCTION can be either a nondeterministic
function, or an ordinary deterministic function.

You must use APPLY-NONDETERMINISTIC to apply a nondeterministic function. An
error is signalled if a nondeterministic function object is used with
CL:APPLY.

You can use APPLY-NONDETERMINISTIC to apply either a deterministic or
nondeterministic function, though even if all of the ARGUMENTS are
deterministic and FUNCTION is a deterministic function object, the call
expression will still be nondeterministic \(with presumably a single value),
since it is impossible to determine at compile time that a given call to
APPLY-NONDETERMINISTIC will be passed only deterministic function objects for
function."
  (declare (ignore function arguments))
  (screamer-error
   "APPLY-NONDETERMINISTIC is a nondeterministic function. As such, it must~%~
   be called only from a nondeterministic context."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline apply-nondeterministic-nondeterministic)))
(cl:defun apply-nondeterministic-nondeterministic
    (continuation function argument &rest arguments)
  (declare (optimize (speed 3) (space 3))
           (function continuation)
           (type (or function nondeterministic-function variable null) function))
  (let ((function (value-of function)))
    (if (nondeterministic-function? function)
        (apply #'apply (nondeterministic-function-function function)
               continuation argument arguments)
        (funcall continuation (apply #'apply function argument arguments)))))

(defun mapcar-nondeterministic-internal (function args)
  ;; Stop collecting values if an argument list is empty
  (unless (some #'null args)
    (cons
     ;; Call `function' on the current set of arguments
     (apply-nondeterministic function (mapcar #'car args))
     ;; Recurse on the remaining arguments
     (mapcar-nondeterministic-internal function (mapcar #'cdr args)))))

(defun mapcar-nondeterministic (function arg &rest args)
  (cond
    ;; Use `mapcar' in deterministic cases for efficiency
    ;; and reduced stack-utilization
    ((not (nondeterministic-function? function)) (apply #'mapcar function arg args))
    ;; Recurse over the elements to nondeterministically
    ;; generate a result
    (t (mapcar-nondeterministic-internal function (cons arg args)))))

(cl:defun multiple-value-call-nondeterministic (function-form &rest values-forms)
  "Analogous to the CL:MULTIPLE-VALUE-CALL, except FUNCTION-FORM can evaluate
to either a nondeterministic function, or an ordinary deterministic function.

You must use MULTIPLE-VALUE-CALL-NONDETERMINISTIC to multiple-value-call a
nondeterministic function. An error is signalled if a nondeterministic function
object is used with CL:MULTIPLE-VALUE-CALL.

You can use MULTIPLE-VALUE-CALL-NONDETERMINISTIC to call either a
deterministic or nondeterministic function, though even if all of the
VALUES-FORMS are deterministic and FUNCTION-FORM evaluates to a deterministic
function object, the call expression will still be nondeterministic \(with
presumably a single value), since it is impossible to determine at compile
time that a given call to MULTIPLE-VALUE-CALL-NONDETERMINISTIC will be passed
only deterministic function objects for function.

While MULTIPLE-VALUE-CALL-NONDETERMINISTIC appears to be a function, it
is really a special-operator implemented by the code-walkers processing
nondeterministic source contexts."
  (declare (ignore function-form values-forms))
  (screamer-error
   "MULTIPLE-VALUE-CALL-NONDETERMINISTIC is a nondeterministic special form. As such,~%~
    it must be called only from a nondeterministic context."))

(defmacro-compile-time multiple-value-bind
    (variables form &body body &environment environment)
  (if (every #'(lambda (form) (deterministic? form environment))
             (peal-off-documentation-string-and-declarations body))
      `(cl:multiple-value-bind ,variables ,form ,@body)
      (let ((other-arguments (gensym "OTHER-")))
        `(multiple-value-call-nondeterministic
          #'(lambda (&optional ,@variables &rest ,other-arguments)
              (declare (ignore ,other-arguments))
              ,@body)
          ,form))))

(defun purge (function-name)
  "Removes any information about FUNCTION-NAME from Screamer's
who-calls database."
  (remhash (value-of function-name) *function-record-table*)
  t)

(defun unwedge-screamer ()
  "Removes any information about all user defined functions from
Screamer's who-calls database."
  (maphash #'(lambda (function-name function-record)
               (unless (function-record-screamer? function-record)
                 (remhash function-name *function-record-table*)))
           *function-record-table*)
  t)

;;; NOTE: These optimized versions of AN-INTEGER, AN-INTEGER-ABOVE,
;;;       AN-INTEGER-BELOW, AN-INTEGER-BETWEEN and A-MEMBER-OF have different
;;;       failure behavior as far as WHEN-FAILING is concerned than the
;;;       original purely Screamer versions. This is likely to affect only
;;;       failure counts generated by COUNT-FAILURES. A small price to pay for
;;;       tail recursion optimization.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-integer))

(cl:defun an-integer ()
  "Generator yielding integers in sequence 0, 1, -1, 2, -2, ..."
  (screamer-error
   "AN-INTEGER is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun an-integer-nondeterministic (continuation)
  (choice-point-external
   (choice-point-internal (funcall continuation 0))
   (let ((i 1))
     (loop (choice-point-internal (funcall continuation i))
           (choice-point-internal (funcall continuation (- i)))
           (incf i)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-integer-above))

(cl:defun an-integer-above (low)
  "Generator yielding integers starting from LOW and continuing sequentially
in increasing direction."
  (declare (ignore low))
  (screamer-error
   "AN-INTEGER-ABOVE is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-above-nondeterministic (continuation low)
  (let ((low (ceiling (value-of low))))
    (choice-point-external
     (let ((i low))
       (loop (choice-point-internal (funcall continuation i))
             (incf i))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-integer-below))

(cl:defun an-integer-below (high)
  "Generator yielding integers starting from HIGH and continuing sequentially
in decreasing direction."
  (declare (ignore high))
  (screamer-error
   "AN-INTEGER-BELOW is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-below-nondeterministic (continuation high)
  (let ((high (floor (value-of high))))
    (choice-point-external
     (let ((i high))
       (loop (choice-point-internal (funcall continuation i))
             (decf i))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-integer-between))

(cl:defun an-integer-between (low high)
  "Nondeterministically returns an integer in the closed interval [LOW, HIGH].
The results are returned in ascending order. Both LOW and HIGH must be
integers. Fails if the interval does not contain any integers."
  (declare (ignore low high))
  (screamer-error
   "AN-INTEGER-BETWEEN is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-between-nondeterministic (continuation low high)
  (let ((low (ceiling (value-of low)))
        (high (floor (value-of high))))
    (unless (> low high)
      (choice-point-external
       (do ((i low (1+ i))) ((= i high))
         (choice-point-internal (funcall continuation i))))
      (funcall continuation high))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-integer-between-prob))

(cl:defun an-integer-between-prob (low high)
  "Nondeterministically returns an integer in the closed interval [LOW, HIGH].
The results are returned in ascending order. Both LOW and HIGH must be
integers. Fails if the interval does not contain any integers."
  (declare (ignore low high))
  (screamer-error
   "AN-INTEGER-BETWEEN-PROB is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-between-prob-nondeterministic (continuation low high)
  (let* ((low (ceiling (value-of low)))
         (high (floor (value-of high)))
         (possibilities (max 0 (1+ (- high low))))
         (prob-avg (if (zerop possibilities) 0 (/ 1 possibilities))))
    (unless (> low high)
      (choice-point-external
       (do ((i low (1+ i))) ((= i high))
         (choice-point-internal
          (progn (trail-prob nil (* (current-probability) prob-avg))
                 (funcall continuation i)))))
      (progn (trail-prob nil (* (current-probability) prob-avg))
             (funcall continuation high)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'a-member-of))

(cl:defun a-member-of (sequence)
  "Nondeterministically returns an element of SEQUENCE. The elements are
returned in the order that they appear in SEQUENCE. The SEQUENCE must be
either a list or a vector."
  (declare (ignore sequence))
  (screamer-error
   "A-MEMBER-OF is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun a-member-of-nondeterministic (continuation sequence)
  (serapeum:nest
   (let* ((sequence (value-of sequence))))
   (macrolet ((call-continuation (cont inp)
                `(funcall ,cont ,inp))))
   (cond
     ((listp sequence)
      (if (null sequence)
          (fail)
          (progn
            (choice-point-external
             (loop (if (null (rest sequence)) (return))
                   (choice-point-internal (call-continuation continuation (first sequence)))
                   (setf sequence (value-of (rest sequence)))))
            (call-continuation continuation (first sequence)))))
     ((vectorp sequence)
      (let ((n (length sequence)))
        (if (zerop n)
            (fail)
            (let ((n (1- n)))
              (choice-point-external
               (dotimes (i n)
                 (choice-point-internal (call-continuation continuation (aref sequence i)))))
              (call-continuation continuation (aref sequence n))))))
     ((serapeum:sequencep sequence)
      (let ((n (length sequence)))
        (if (zerop n)
            (fail)
            (let ((n (1- n)))
              (choice-point-external
               (dotimes (i n)
                 (choice-point-internal (call-continuation continuation (elt sequence i)))))
              (call-continuation continuation (elt sequence n))))))
     (t (error "SEQUENCE must be a sequence")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'a-member-of-prob))

(cl:defun a-member-of-prob (sequence)
  "Nondeterministically returns an element of SEQUENCE. The elements are
returned in the order that they appear in SEQUENCE."
  (declare (ignore sequence))
  (screamer-error
   "A-MEMBER-OF-PROB is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun a-member-of-prob-nondeterministic (continuation sequence)
  (serapeum:nest
   (let* ((sequence (value-of sequence))
          (seq-count (length sequence))
          (prob-avg (if (zerop seq-count) 0 (/ 1 seq-count)))))
   (macrolet ((call-continuation (cont inp)
                `(progn
                   (trail-prob nil (* (current-probability) prob-avg))
                   (funcall ,cont ,inp)))))
   (cond
     ((listp sequence)
      (unless (null sequence)
        (choice-point-external
         (loop (if (null (rest sequence)) (return))
               (choice-point-internal (call-continuation continuation (first sequence)))
               (setf sequence (value-of (rest sequence)))))
        (call-continuation continuation (first sequence))))
     ((vectorp sequence)
      (let ((n (length sequence)))
        (unless (zerop n)
          (let ((n (1- n)))
            (choice-point-external
             (dotimes (i n)
               (choice-point-internal (call-continuation continuation (aref sequence i)))))
            (call-continuation continuation (aref sequence n))))))
     ((serapeum:sequencep sequence)
      (let ((n (length sequence)))
        (unless (zerop n)
          (let ((n (1- n)))
            (choice-point-external
             (dotimes (i n)
               (choice-point-internal (call-continuation continuation (elt sequence i)))))
            (call-continuation continuation (aref sequence n))))))
     (t (error "SEQUENCE must be a sequence")))))

(cl:defun collection-to-sequence (coll)
  "Converts various collection types to sequences"
  (etypecase coll
    (sequence coll)
    (array
     (serapeum:with-collector (collect)
       (dotimes (i (array-total-size coll))
         (collect (row-major-aref coll i)))))
    (hash-table (hash-table-keys coll))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-element-of))

(cl:defun an-element-of (collection)
  "Nondeterministically returns an element of COLLECTION. The elements are
returned in the order that they appear in COLLECTION.

If COLLECTION is a sequence, this is equivalent to A-MEMBER-OF.
If COLLECTION is a multidimensional array, the elements are selected in
row-major order.
If COLLECTION is a hash-table, the keys are derived via `alxandria:hash-table-keys'.
Note that this may not maintain key order."
  (declare (ignore collection))
  (screamer-error
   "AN-ELEMENT-OF is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun an-element-of-nondeterministic (continuation collection
                                          &aux (collection (value-of collection)))
  (a-member-of-nondeterministic continuation (collection-to-sequence collection)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'sample))

(cl:defun sample (source &key count (stop nil stop-supplied) (test 'eql))
  "Continuously samples random values from SOURCE.

If SOURCE is a list, it is a plist where the keys are possible
return values and the values are the probabilities of each
value. Values will be normalized to sum to one, i.e.
'((hi 3) (bye 7)) gives 0.3 probability to hi and 0.7 to bye.

If SOURCE is a function, it will be called to return a 2-member
list with the first element being the value and the second being
a number between 0 and 1 representing its probability.

If COUNT is nil, only a single value will be sampled per attempt.
If it is a non-negative integer, then a list of samples will be
returned. In this case samples are treated as probabilistically
independent, i.e. the chance of the returned list is the product
of the chance of each sample taken.

NOTE: Execution will NOT backtrack past a SAMPLE statement!
Ensure you limit the number of values you request, and do not
expect choice points before a sample to be backtracked to!

If you want to use SAMPLE inside a larger nondeterministic
block, it may be useful to wrap it in its own ALL-VALUES,
ALL-VALUES-PROB, N-VALUES, N-VALUES-PROB, ONE-VALUE, or
similar form."
  (declare (ignore source count stop stop-supplied test))
  (screamer-error
   "SAMPLE is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun sample-nondeterministic (continuation source &key count (stop nil stop-supplied) (test 'eql))
  (declare (function continuation) (type (or (integer 0) null) count))
  (s:nest
   (flet ((normalize (d psum)
            (declare (number psum))
            ;; Normalize a list-distribution given the sum
            ;; of the probabilities
            (mapcar (lambda (c)
                      (list (first c)
                            (/ (second c)
                               psum)))
                    d))))
   ;; Normalize the input distribution if its a plist
   (let ((source (typecase source
                   ((or function nondeterministic-function null) source)
                   (cons
                    (normalize source
                               (reduce #'+ source
                                       :key #'second)))))))
   ;; Get an individual sample from a distribution
   ;; Returns a list of the value and the probability
   (flet ((sample-internal (source)
            (typecase source
              ((or function nondeterministic-function)
               (let* ((ret nil) (local-cont (lambda (inp) (push inp ret))))
                 (catch '%fail
                   (funcall-nondeterministic-nondeterministic local-cont source))
                 ;; Fail unless we have some outputs
                 (unless ret (fail))
                 ;; FIXME: Need to figure out how to use all values in ret so that
                 ;; we can use nondeterministic `source' functions in `sample'
                 (nreverse ret)))
              (list
               (let* ((probs (mapcar #'second source))
                      (selection (random 1.0))
                      (index (iter:iter
                               (iter:with sum = 0)
                               (iter:for i from 0)
                               ;; Note: probabilities must be normalized
                               (iter:for p in probs)
                               (incf sum p)
                               ;; Stop when we pass the random selection
                               (iter:while (< sum selection))
                               ;; Return the current index
                               (iter:finally (return i)))))
                 (list probs)
                 ;; Return a 1-element list to match the format of
                 ;; function `source's
                 (list (nth index source))))))
          (check-stop (val) (and stop-supplied (funcall test val stop))))
     (declare (inline sample-internal check-stop)))
   (block screamer-sample-block)
   ;; Syntax sugar for updating probabilities and calling CONTINUATION
   (macrolet ((call-continuation (cont inp)
                `(progn
                   (when (check-stop ,inp) (return-from screamer-sample-block))
                   (trail-prob nil (* (current-probability)
                                      (second ,inp)))
                   (funcall ,cont (first ,inp))))))
   ;; FIXME: Need to fix both paths to deal with `sample-internal'
   ;; providing multple possible values
   ;; NOTE: Maybe make count recursive instead? And then
   ;; call continuation at the end when count is 0?
   (typecase count
     ;; When count is provided
     (non-negative-integer
      (s:nest
       (choice-point-external)
       (loop)
       ;; Keep sampling with every loop iteration
       (choice-point-internal)
       ;; Get the list of values and multiply their probabilities together
       (let ((ret (iter:iter (iter:for i below count)
                    (iter:for (s sp) = (first (sample-internal source)))
                    (iter:collect s into members)
                    (iter:multiply sp into prob)
                    (iter:finally (return (list members prob)))))))
       ;; Call the continuation with the given values
       (call-continuation continuation)
       ret))
     ;; When count is absent
     (null
      (s:nest
       (choice-point-external)
       (loop)
       (choice-point-internal)
       (let ((ret (first (sample-internal source)))))
       (call-continuation continuation)
       ret)))))

(defun sample-once (source &key count)
  "Like `sample', but does not create a choice point, meaning it is
not backtracked to and does not block backtracking to earlier choice
points."
  (let ((result (first (n-values-prob (1) (sample source :count count)))))
    (unless result (fail))
    (factor-prob (second result))
    (first result)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'sample-optimizing))

;; FIXME: This is similar to `best-value', and designing the correct semantics
;; is more complicated than I initially gave it credit for
(cl:defun sample-optimizing (source &key (stop nil stop-supplied) (test 'eql))
  "EXPERIMENTAL
Continuously samples values from SOURCE.

SOURCE must be a function which takes as input a list of prior outputs
from the next enclosing `-VALUES' or `-VALUES-PROB' form (i.e. the current
value of `screamer::*screamer-results*'). This may be `nil'. Note that
this means the input will be different in probabilistic vs non-probabilistic
forms, as the former collect lists of values and corresponding probabilities.

Every time this choice point is encountered, SAMPLE will be called and
its result will be interpreted as a list of possible outputs, represented
as 2-element lists of a value and a probability. For each possible output,
the value will be used as a potential output of this form, and the
probability will modify the value of `current-probability'. The probabilities
should be numbers between 0 and 1. If an element of the output of SAMPLE is
not a list, then it will be wrapped in a list with probability element 1.

If STOP is explicitly provided as an argument, each output of SAMPLE will
be checked against STOP using TEST (i.e. (funcall test sample stop)). If
the output of this is non-`nil', this form will `fail' rather than giving
an output."
  (declare (ignore source stop stop-supplied test))
  (screamer-error
   "SAMPLE-OPTIMIZING is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun sample-optimizing-nondeterministic (continuation source &key (stop nil stop-supplied) (test 'eql))
  (s:nest
   (flet ((ensure-prob-list (e)
            (typecase e
              (list e)
              (t (list e 1)))))
     (declare (inline ensure-prob-list)))
   (flet ((sample-internal ()
            ;; (print "sample-start")
            ;; (format t "~%current results: ~A" *screamer-results*)
            (let ((ans (s:nest
                        (mapcar #'ensure-prob-list)
                        (funcall source *screamer-results*))))
              ;; (format t "~%output of sample: ~A" ans)
              ans))
          (check-stop (val) (and stop-supplied (funcall test val stop))))
     (declare (inline sample-internal check-stop)))
   (macrolet ((call-continuation (cont inp)
                (with-gensyms (val)
                  `(let ((,val ,inp))
                     ;; (print ,val)
                     (trail-prob nil (* (current-probability)
                                        (second ,val)))
                     (funcall ,cont (first ,val)))))))
   (choice-point-external)
   (block sample-optimizing-block)
   (iter:iter
     (iter:for possibilities = (sample-internal))
     (iter:while possibilities))
   (iter:iter (iter:for poss in possibilities))
   (if (check-stop poss) (return-from sample-optimizing-block))
   (choice-point-internal)
   (call-continuation continuation poss)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'state-transition))

(cl:defun state-transition (state-machine current-state &optional (times 1))
  "Transitions from the current state to all possible next states.

STATE-MACHINE may be an alist with keys being states and values being
plists of state-probability pairs of the transition states.

STATE-MACHINE may instead be a single-argument function which takes a state
as input and returns a plist of state-probability pairs of the transition states.

These transition probabilities must be positive numbers summing to 1 for each
state (measured per `SCREAMER::ROUGHLY-='). If STATE-MACHINE is an alist this
will be verified. If it is a function, then the user is expected to assure this
condition.

When STATE-MACHINE is an alist, note that you can transition to a state which
was not specified in the alist, in which case it will be treated as a node with
no successors.

TIMES must be a non-negative integer."
  (declare (ignore state-machine current-state times))
  (screamer-error
   "STATE-TRANSITION is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun state-transition-nondeterministic
    (continuation state-machine current-state &optional (times 1))
  (assert (typep times 'non-negative-integer))
  (serapeum:nest
   (let* ((alist-machine (typecase state-machine (cons t)))
          (test (or (and alist-machine
                         (cond ((every (compose (rcurry #'typep 'symbol)
                                                #'first)
                                       state-machine)
                                'eq)
                               ((every (compose (rcurry #'typep '(or symbol character))
                                                #'first)
                                       state-machine)
                                'eql)
                               ((every (compose (rcurry #'typep 'number)
                                                #'first)
                                       state-machine)
                                ;; Note: not using '= because there might be unlisted
                                ;; states with non-numeric values
                                ;; Example: '((nil (nil 0.99) (t 0.01)))
                                'eq)))
                    'equal))))
   (labels ((get-state (state machine)
              (typecase machine
                (list
                 (or (assoc state machine :test test)
                     ;; If not found, loop to the same state
                     (list state (list state 1))))
                (function
                 (cons state (funcall machine state)))))))
   ;; For alist state-machines, fail unless all transition-probability sets sum to 1
   (if (and alist-machine
            (some (s:nest
                   (lambda (state-spec))
                   (let* ((transitions (rest state-spec))
                          (prob-sum (reduce (lambda (a b)
                                              (+ (second b) a))
                                            transitions
                                            :initial-value 0))))
                   (or (emptyp transitions)
                       (not (roughly-= prob-sum 1))
                       (not (every (compose (curry #'<= 0)
                                            #'second)
                                   transitions))))
                  state-machine))
       (fail))

   (let ((recursion-check-interval (s:nest
                                    (ash 16)
                                    (max 1)
                                    (integer-length)
                                    (float-precision *numeric-bounds-collapse-threshold*)))))
   (labels ((recurse-transitions (start &optional (n 1))
              ;; Get the starting probability distribution
              (let ((state-probs (list (list start 1)))
                    ;; Track the next probability distribution
                    (new-probs nil))
                (s:nest
                 ;; Allow short-circuiting if transitions have stabilized.
                 (block short-circuit)
                 ;; Gets the probabilities in a distribution
                 ;; so we can increment them
                 (labels ((get-new-prob (state)
                            (or (assoc state new-probs :test test)
                                (let ((c (list state 0)))
                                  (push c new-probs)
                                  c)))))

                 ;; Recurse over the state machine n times
                 (iter:iter (iter:for i from 1 to n)
                   (setf new-probs nil))

                 (progn
                   ;; Iterate over currently-possible states
                   (iter:iter (iter:for (s p) in state-probs)
                     (iter:for s-spec = (get-state s state-machine))

                     ;; Get the follow-up states and each of their odds
                     (iter:for s-trans = (cdr s-spec))

                     (iter:iterate (iter:for (targ targ-p) in s-trans)
                       ;; Get the currently-tracked probability for the target
                       (iter:for targ-new-prob = (get-new-prob targ))
                       ;; Increment the probability by the current probability
                       ;; times the odds of the transition
                       (incf (second targ-new-prob) (* p targ-p))))

                   ;; If the state machine stabilizes, return early
                   ;; NOTE: We don't do this for function machines,
                   ;; as we don't know what latent variables are involved
                   ;; in generating the function output.
                   (when (and alist-machine
                              (zerop (mod i recursion-check-interval))
                              (equal state-probs new-probs))
                     (mapc #'list state-probs)
                     (list state-probs)
                     (setf state-probs new-probs)
                     (return-from short-circuit))

                   ;; Replace the old probability state with the new one
                   (mapc #'list state-probs)
                   (list state-probs)
                   (setf state-probs new-probs)))

                ;; Return the state probabilities after everything
                state-probs))))
   (let* ((transitions (recurse-transitions current-state
                                            times))
          (transitions (sort transitions #'> :key #'second))))

   ;; Implement choice point over final set of possible states
   (choice-point-external)
   (dolist (next transitions))
   (choice-point-internal)
   (progn (trail-prob nil (* (current-probability) (second next)))
          (funcall continuation (first next)))))


;;; NOTE: The following two functions work only when Screamer is running under
;;;       ILisp/GNUEmacs with iscream.el loaded.

(defun emacs-eval (expression)
  (unless *iscream?*
    (error "Cannot do EMACS-EVAL unless Screamer is running under~%~
          ILisp/GNUEmacs with iscream.el loaded."))
  (format *terminal-io* "~A~A~A"
          (format nil "~A" (code-char 27))
          (string-downcase (format nil "~A" expression))
          (format nil "~A" (code-char 29))))

(defmacro-compile-time local-output (&body forms)
  "Currently unsupported.

When running under ILisp with iscream.el loaded, does non-determinism aware
output to Emacs, which will be deleted when the current choice is unwound."
  `(progn
     (unless *iscream?*
       (error "Cannot do LOCAL-OUTPUT unless Screamer is running under~%~
            ILisp/GNUEmacs with iscream.el loaded."))
     (trail #'(lambda () (emacs-eval '(pop-end-marker))))
     (emacs-eval '(push-end-marker))
     ,@forms))

;;; Constraints

(defvar *name* 0 "The counter for anonymous names.")

(defvar *minimum-shrink-ratio* 1e-2
  "Ignore propagations which reduce the range of a variable by less than this
ratio.")

(defvar *maximum-discretization-range* 20
  "Discretize integer variables whose range is not greater than this number.
Discretize all integer variables if NIL. Must be an integer or NIL.

For some `p-screamer' constructs that can produce an unbounded count of values,
this variable is also used to determine how many values are run in parallel at
a time.")

(defvar *strategy* :gfc
  "Strategy to use for FUNCALLV and APPLYV. Either :GFC for Generalized
Forward Checking, or :AC for Arc Consistency. Default is :GFC.")

;;; NOTE: Enable this to use CLOS instead of DEFSTRUCT for variables.
#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :screamer-clos *features* :test #'eq))

;;; TODO: Figure out how to track atoms that a variable
;;; is dependent on, and its relationship to said atoms
;;; (e.g. relative lower/upper bounds, etc)
;;; This is necessary both for probabilistic reasoning by
;;; weighted model counting, as well as for correct
;;; lifted logical inference (e.g. in cases where the same
;;; variable appears multiple times in an arithmetic
;;; structure).
;;; NOTE: How would this actually be /used/ to resolve
;;; the second case? It seems that if grounding would
;;; be a valid approach then it would already be done
;;; via the noticer system...
#-screamer-clos
(defstruct-compile-time (variable (:print-function print-variable)
                                  (:predicate variable?)
                                  (:constructor make-variable-internal))
  name
  (noticers nil :type list)
  (operation nil :type (or symbol null))
  (dependencies nil :type list)
  (type nil :type list)
  (enumerated-domain t :type (or boolean list))
  (enumerated-antidomain nil :type (or boolean list))
  value
  (possibly-integer? t :type boolean)
  (possibly-noninteger-real? t :type boolean)
  (possibly-nonreal-number? t :type boolean)
  (possibly-boolean? t :type boolean)
  (possibly-nonboolean-nonnumber? t :type boolean)
  (lower-bound nil :type (or null number))
  (upper-bound nil :type (or null number)))

#+screamer-clos
(defclass variable ()
  ((name :accessor variable-name :initarg :name)
   (noticers :accessor variable-noticers :initform nil)
   (operation :accessor variable-operation :initform nil)
   (dependencies :accessor variable-dependencies :initform nil)
   (type :accessor variable-type :initform nil)
   (enumerated-domain :accessor variable-enumerated-domain :initform t)
   (enumerated-antidomain :accessor variable-enumerated-antidomain
                          :initform nil)
   (value :accessor variable-value)
   (possibly-integer? :accessor variable-possibly-integer? :initform t)
   (possibly-noninteger-real? :accessor variable-possibly-noninteger-real?
                              :initform t)
   (possibly-nonreal-number? :accessor variable-possibly-nonreal-number?
                             :initform t)
   (possibly-boolean? :accessor variable-possibly-boolean? :initform t)
   (possibly-nonboolean-nonnumber?
    :accessor variable-possibly-nonboolean-nonnumber?
    :initform t)
   (lower-bound :accessor variable-lower-bound :initform nil)
   (upper-bound :accessor variable-upper-bound :initform nil)))

;;; Helpers to interact with variable objects
#+screamer-clos
(defmethod print-object ((variable variable) stream)
  (print-variable variable stream nil))

#+screamer-clos
(defun-compile-time variable? (thing) (typep thing 'variable))

(defun-compile-time variable-enumerated-domain-type (var)
  (declare (optimize (speed 3) (space 3) (debug 0)))
  (when (and (variable? var)
             ;; Has non-null enumerated domain
             (variable-enumerated-domain var)
             ;; Has an actual list of enumerated values
             (listp (variable-enumerated-domain var)))
    ;; Return an or type containing the different values
    `(or ,@(mapcar (serapeum:op `(value ,_)) (variable-enumerated-domain var)))))

(defun-compile-time noticer-member (val var)
  (declare (function val) (variable var))
  (iter:iter
    (iter:for i in (variable-noticers var))
    (when (typecase i (function (eq val i)))
      (return t))))


(cl:defun integers-between (low high)
  (cond ((and (typep low 'fixnum) (typep high 'fixnum))
         ;; KLUDGE: Don't change this to a LOOP, since in some implementations
         ;; (eg. Lispworks) non-trivial LOOP generates MACROLETs that can't be
         ;; supported by WALK.
         (do ((result nil)
              (i low (1+ i)))
             ((> i high) (nreverse result))
           (declare (type fixnum i))
           (push i result)))
        (t
         ;; KLUDGE: As above.
         (do ((result nil)
              (i low (1+ i)))
             ((> i high) (nreverse result))
           (push i result)))))

(cl:defun booleanp (x)
  "Returns true iff X is T or NIL."
  (typep x 'boolean))

(cl:defun infinity-min (x y) (and x y (min x y)))

(cl:defun infinity-max (x y) (and x y (max x y)))

(cl:defun infinity-+ (x y) (and x y (+ x y)))

(cl:defun infinity-- (x y) (and x y (- x y)))

(cl:defun infinity-* (x y) (and x y (* x y)))

(cl:defun contains-variables? (x)
  (declare (optimize (speed 3) (space 3)))
  (typecase x
    (cons (or (contains-variables? (car x)) (contains-variables? (cdr x))))
    (vector (some #'contains-variables? x))
    (variable t)
    (otherwise nil)))

(cl:defun eliminate-variables (x)
  (declare (optimize (speed 3) (space 3)))
  (if (contains-variables? x)
      (typecase x
        (cons (cons (eliminate-variables (car x)) (eliminate-variables (cdr x))))
        (vector (map 'vector #'eliminate-variables x))
        (t (eliminate-variables (value-of x))))
      x))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-integer?)))
(cl:defun variable-integer? (x)
  (and (not (variable-possibly-boolean? x))
       (not (variable-possibly-nonboolean-nonnumber? x))
       (not (variable-possibly-nonreal-number? x))
       (not (variable-possibly-noninteger-real? x))
       (variable-possibly-integer? x)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-noninteger?)))
(cl:defun variable-noninteger? (x)
  (and (or (variable-possibly-boolean? x)
           (variable-possibly-nonboolean-nonnumber? x)
           (variable-possibly-nonreal-number? x)
           (variable-possibly-noninteger-real? x))
       (not (variable-possibly-integer? x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-real?)))
(cl:defun variable-real? (x)
  (and (not (variable-possibly-boolean? x))
       (not (variable-possibly-nonboolean-nonnumber? x))
       (not (variable-possibly-nonreal-number? x))
       (or (variable-possibly-noninteger-real? x)
           (variable-possibly-integer? x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-nonreal?)))
(cl:defun variable-nonreal? (x)
  (and (or (variable-possibly-boolean? x)
           (variable-possibly-nonboolean-nonnumber? x)
           (variable-possibly-nonreal-number? x))
       (not (variable-possibly-noninteger-real? x))
       (not (variable-possibly-integer? x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-number?)))
(cl:defun variable-number? (x)
  (and (not (variable-possibly-boolean? x))
       (not (variable-possibly-nonboolean-nonnumber? x))
       (or (variable-possibly-nonreal-number? x)
           (variable-possibly-noninteger-real? x)
           (variable-possibly-integer? x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-nonnumber?)))
(cl:defun variable-nonnumber? (x)
  (and (or (variable-possibly-boolean? x)
           (variable-possibly-nonboolean-nonnumber? x))
       (not (variable-possibly-nonreal-number? x))
       (not (variable-possibly-noninteger-real? x))
       (not (variable-possibly-integer? x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-boolean?)))
(cl:defun variable-boolean? (x)
  (and (variable-possibly-boolean? x)
       (not (variable-possibly-nonboolean-nonnumber? x))
       (not (variable-possibly-nonreal-number? x))
       (not (variable-possibly-noninteger-real? x))
       (not (variable-possibly-integer? x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-nonboolean?)))
(cl:defun variable-nonboolean? (x)
  (and (not (variable-possibly-boolean? x))
       (or (variable-possibly-nonboolean-nonnumber? x)
           (variable-possibly-nonreal-number? x)
           (variable-possibly-noninteger-real? x)
           (variable-possibly-integer? x))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-true?)))
(cl:defun variable-true? (x) (eq (variable-value x) t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline variable-false?)))
(cl:defun variable-false? (x) (null (variable-value x)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline value-of)))
(cl:defun value-of (x)
  "Returns X if X is not a variable. If X is a variable then VALUE-OF
dereferences X and returns the dereferenced value. If X is bound then
the value returned will not be a variable. If X is unbound then the
value returned will be a variable which may be X itself or another
variable which is shared with X."
  (declare (optimize (speed 3) (space 3) (debug 0)))
  (tagbody
   loop
     (when (or (not (variable? x))
               #+screamer-clos (not (slot-boundp x 'value))
               (eq x (setf x (variable-value x))))
       (return-from value-of x))
     ;; (setf x (variable-value x))
     (go loop)))

(defun print-variable (x stream print-level)
  (declare (ignore print-level)
           (stream stream))
  (let ((x (value-of x)))
    (cond
      ((variable? x)
       (if (and (not (eq (variable-enumerated-domain x) t))
                (not (null (variable-enumerated-antidomain x))))
           (error "This shouldn't happen"))
       (format stream "[~S" (variable-name x))
       (format stream "~A"
               (cond ((variable-boolean? x) " Boolean")
                     ((variable-integer? x) " integer")
                     ((variable-real? x)
                      (if (variable-noninteger? x) " noninteger-real" " real"))
                     ((variable-number? x)
                      (cond ((variable-nonreal? x) " nonreal-number")
                            ((variable-noninteger? x) " noninteger-number")
                            (t " number")))
                     ((variable-nonnumber? x) " nonnumber")
                     ((variable-nonreal? x) " nonreal")
                     ((variable-noninteger? x) " noninteger")
                     (t "")))
       (if (variable-real? x)
           (if (variable-lower-bound x)
               (if (variable-upper-bound x)
                   (format stream " ~D:~D"
                           (variable-lower-bound x) (variable-upper-bound x))
                   (format stream " ~D:" (variable-lower-bound x)))
               (if (variable-upper-bound x)
                   (format stream " :~D" (variable-upper-bound x)))))
       (if (and (not (eq (variable-enumerated-domain x) t))
                (not (variable-boolean? x)))
           (format stream " enumerated-domain:~S"
                   (variable-enumerated-domain x)))
       (if (not (null (variable-enumerated-antidomain x)))
           (format stream " enumerated-antidomain:~S"
                   (variable-enumerated-antidomain x)))
       (format stream "]"))
      (t (format stream "~S" x)))))

(defun make-variable (&optional (name nil name?))
  "Creates and returns a new variable. Variables are assigned a name
which is only used to identify the variable when it is printed. If the
parameter NAME is given then it is assigned as the name of the
variable. Otherwise, a unique name is assigned. The parameter NAME can
be any Lisp object."
  (let ((variable
          #-screamer-clos
          (make-variable-internal :name (if name? name (incf *name*)))
          #+screamer-clos
          (make-instance 'variable :name (if name? name (incf *name*)))))
    (setf (variable-value variable) variable)
    variable))

(defun variablize (x)
  (if (variable? x)
      (tagbody
       loop
         (if (or (not (variable? (variable-value x)))
                 (eq (variable-value x) x))
             (return-from variablize x))
         (setf x (variable-value x))
         (go loop))
      (let ((y (make-variable))) (restrict-value! y x) y)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline bound?)))
(cl:defun bound? (x)
  "Returns T if X is not a variable or if X is a bound variable. Otherwise
returns NIL. BOUND? is analogous to the extra-logical predicates `var' and
`nonvar' typically available in Prolog."
  (not (variable? (value-of x))))

(cl:defun bounded? (x)
  "Returns true if there are finite possible values for X."
  (or (bound? x)
      (or (variable-boolean? x)
          (not (serapeum:in (variable-enumerated-domain x) t nil))
          (and
           (variable-integer? x)
           (variable-lower-bound x)
           (variable-upper-bound x)))))

(cl:defun ground? (x)
  "The primitive GROUND? is an extension of the primitive BOUND? which
can recursively determine whether an entire aggregate object is
bound. Returns T if X is bound and either the value of X is atomic or
a CONS tree where all atoms are bound.

Otherwise returns nil."
  (let ((x (value-of x)))
    (and (not (variable? x))
         (or (not (consp x))
             (and (ground? (car x))
                  (ground? (cdr x)))))))

(cl:defun grounded? (x)
  "Similar to GROUND?, but extending BOUNDED? instead of BOUND?
NOTE: This may not work correctly on variables whose enumerated
domain includes structures that themselves contain variables."
  (and (bounded? x)
       (or (not (consp x))
           (and (grounded? (car x))
                (grounded? (cdr x))))))

(defun apply-substitution (x)
  "If X is a SEQUENCE or HASH-TABLE, returns a freshly consed
copy of the tree with all variables dereferenced.
Otherwise returns the value of X."
  (let ((x (value-of x)))
    (etypecase x
      (cons (if (null (cdr (last x)))
                ;; If terminates with nil (ie normal list)
                ;; use mapcar to not consume stack
                (mapcar #'apply-substitution x)
                ;; Otherwise recurse on the car and cdr
                (cons (apply-substitution (car x))
                      (apply-substitution (cdr x)))))
      (string x)
      (simple-vector (map 'vector #'apply-substitution x))
      (sequence (let ((copy (copy-seq x)))
                  (dotimes (idx (length x))
                    (setf (elt copy idx)
                          (apply-substitution (elt x idx))))
                  copy))
      (array (let ((arr (copy-array x)))
               (dotimes (idx (array-total-size arr))
                 (setf (row-major-aref arr idx)
                       (apply-substitution (row-major-aref arr idx))))
               arr))
      (hash-table
       (let ((x (copy-hash-table x)))
         (maphash (lambda (k v) (setf (gethash k x) (apply-substitution v))) x)
         x))
      (t x))))

(cl:defun occurs-in? (x value)
  ;; NOTE: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; NOTE: Will loop if VALUE is circular.
  (cond
    ((eq x value) t)
    ((and (variable? value) (not (eq value (variable-value value))))
     (occurs-in? x (variable-value value)))
    ((consp value) (or (occurs-in? x (car value)) (occurs-in? x (cdr value))))
    ((s:sequencep value) (some (lambda (val) (occurs-in? x val)) value))
    ((arrayp value) (block occurs-in
                      (dotimes (idx (array-total-size value))
                        (when (occurs-in? x (row-major-aref value idx))
                          (return-from occurs-in t)))))
    ((hash-table-p value) (occurs-in? x (hash-table-values value)))
    (t nil)))

(defun attach-dependencies!-internal (dependencies x)
  (when (variable? x)
    (dolist (dep dependencies)
      (if (eql dep x)
          (screamer-error "~A cannot depend on itself!" x)
          (when (variable? dep)
            (pushnew dep (variable-dependencies x)))))))

(defun attach-noticer!-internal (noticer x)
  ;; NOTE: Will loop if X is circular.
  (s:nest
   (typecase x
     (cons
      (attach-noticer!-internal noticer (car x))
      (attach-noticer!-internal noticer (cdr x))))
   (variable)
   (if (eq x (variable-value x))
       ;; NOTE: I can't remember why this check for duplication is
       ;;       here.
       (unless (noticer-member noticer x)
         ;; NOTE: This can't be a PUSH because of the Lucid screw.
         (local (setf (variable-noticers x)
                      (cons noticer (variable-noticers x)))))
       (attach-noticer!-internal noticer (variable-value x)))))

(defun attach-noticer! (noticer x &key dependencies)
  ;; Track dependency variables if provided
  (when dependencies
    (attach-dependencies!-internal dependencies x))

  (when noticer
    (attach-noticer!-internal noticer x)
    (funcall noticer)))

(defun run-noticers (x)
  (dolist (noticer (variable-noticers x)) (funcall noticer)))

;;; Restrictions

(defun restrict-integer! (x)
  ;; NOTE: X must be a variable.
  (unless (variable-possibly-integer? x) (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-noninteger-real? x)
          (local (setf (variable-possibly-noninteger-real? x) nil))
          (setf run? t))
        (when (variable-possibly-nonreal-number? x)
          (local (setf (variable-possibly-nonreal-number? x) nil))
          (setf run? t))
        (when (variable-possibly-boolean? x)
          (local (setf (variable-possibly-boolean? x) nil))
          (setf run? t))
        (when (variable-possibly-nonboolean-nonnumber? x)
          (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
          (setf run? t))
        (when (and (variable-lower-bound x)
                   (not (integerp (variable-lower-bound x))))
          (if (and (variable-upper-bound x)
                   (< (variable-upper-bound x)
                      (ceiling (variable-lower-bound x))))
              (fail))
          (local (setf (variable-lower-bound x)
                       (ceiling (variable-lower-bound x))))
          (setf run? t))
        (when (and (variable-upper-bound x)
                   (not (integerp (variable-upper-bound x))))
          (if (and (variable-lower-bound x)
                   (> (variable-lower-bound x)
                      (floor (variable-upper-bound x))))
              (fail))
          (local (setf (variable-upper-bound x) (floor (variable-upper-bound x))))
          (setf run? t))
        (when run?
          (cond ((eq (variable-enumerated-domain x) t)
                 (if (and (variable-lower-bound x)
                          (variable-upper-bound x)
                          (or (null *maximum-discretization-range*)
                              (<= (- (variable-upper-bound x)
                                     (variable-lower-bound x))
                                  *maximum-discretization-range*)))
                     (set-enumerated-domain!
                      x (integers-between
                         (variable-lower-bound x)
                         (variable-upper-bound x)))))
                ((not (every #'integerp (variable-enumerated-domain x)))
                 ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
                 ;;       This would also allow checking list only once.
                 (set-enumerated-domain!
                  x (remove-if-not #'integerp (variable-enumerated-domain x)))))
          (run-noticers x)))))

(defun restrict-noninteger! (x)
  ;; NOTE: X must be a variable.
  (unless (or (variable-possibly-noninteger-real? x)
              (variable-possibly-nonreal-number? x)
              (variable-possibly-boolean? x)
              (variable-possibly-nonboolean-nonnumber? x))
    (fail))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (variable-possibly-integer? x))
    (local (setf (variable-possibly-integer? x) nil))
    (if (and (not (eq (variable-enumerated-domain x) t))
             (some #'integerp (variable-enumerated-domain x)))
        ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
        ;;       This would also allow checking list only once.
        (set-enumerated-domain!
         x (remove-if #'integerp (variable-enumerated-domain x))))
    (run-noticers x)))

(defun restrict-real! (x)
  ;; NOTE: X must be a variable.
  (unless (or (variable-possibly-integer? x)
              (variable-possibly-noninteger-real? x))
    (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-nonreal-number? x)
          (local (setf (variable-possibly-nonreal-number? x) nil))
          (setf run? t))
        (when (variable-possibly-boolean? x)
          (local (setf (variable-possibly-boolean? x) nil))
          (setf run? t))
        (when (variable-possibly-nonboolean-nonnumber? x)
          (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
          (setf run? t))
        (when run?
          (if (and (not (eq (variable-enumerated-domain x) t))
                   (not (every #'realp (variable-enumerated-domain x))))
              ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
              ;;       This would also allow checking list only once.
              (set-enumerated-domain!
               x (remove-if-not #'realp (variable-enumerated-domain x))))
          (run-noticers x)))))

(defun restrict-nonreal! (x)
  ;; NOTE: X must be a variable.
  (unless (or (variable-possibly-nonreal-number? x)
              (variable-possibly-boolean? x)
              (variable-possibly-nonboolean-nonnumber? x))
    (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-integer? x)
          (local (setf (variable-possibly-integer? x) nil))
          (setf run? t))
        (when (variable-possibly-noninteger-real? x)
          (local (setf (variable-possibly-noninteger-real? x) nil))
          (setf run? t))
        (when run?
          (if (and (not (eq (variable-enumerated-domain x) t))
                   (some #'realp (variable-enumerated-domain x)))
              ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
              ;;       This would also allow checking list only once.
              (set-enumerated-domain!
               x (remove-if #'realp (variable-enumerated-domain x))))
          (run-noticers x)))))

(defun restrict-number! (x)
  ;; NOTE: X must be a variable.
  (unless (or (variable-possibly-integer? x)
              (variable-possibly-noninteger-real? x)
              (variable-possibly-nonreal-number? x))
    (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-boolean? x)
          (local (setf (variable-possibly-boolean? x) nil))
          (setf run? t))
        (when (variable-possibly-nonboolean-nonnumber? x)
          (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
          (setf run? t))
        (when run?
          (if (and (not (eq (variable-enumerated-domain x) t))
                   (not (every #'numberp (variable-enumerated-domain x))))
              ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
              ;;       This would also allow checking list only once.
              (set-enumerated-domain!
               x (remove-if-not #'numberp (variable-enumerated-domain x))))
          (run-noticers x)))))

(defun restrict-nonnumber! (x)
  ;; NOTE: X must be a variable.
  (unless (or (variable-possibly-boolean? x)
              (variable-possibly-nonboolean-nonnumber? x))
    (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-integer? x)
          (local (setf (variable-possibly-integer? x) nil))
          (setf run? t))
        (when (variable-possibly-noninteger-real? x)
          (local (setf (variable-possibly-noninteger-real? x) nil))
          (setf run? t))
        (when (variable-possibly-nonreal-number? x)
          (local (setf (variable-possibly-nonreal-number? x) nil))
          (setf run? t))
        (when run?
          (if (and (not (eq (variable-enumerated-domain x) t))
                   (some #'numberp (variable-enumerated-domain x)))
              ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
              ;;       This would also allow checking list only once.
              (set-enumerated-domain!
               x (remove-if #'numberp (variable-enumerated-domain x))))
          (run-noticers x)))))

(defun restrict-boolean! (x)
  ;; NOTE: X must be a variable.
  (unless (variable-possibly-boolean? x) (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-integer? x)
          (local (setf (variable-possibly-integer? x) nil))
          (setf run? t))
        (when (variable-possibly-noninteger-real? x)
          (local (setf (variable-possibly-noninteger-real? x) nil))
          (setf run? t))
        (when (variable-possibly-nonreal-number? x)
          (local (setf (variable-possibly-nonreal-number? x) nil))
          (setf run? t))
        (when (variable-possibly-nonboolean-nonnumber? x)
          (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
          (setf run? t))
        (when run?
          (cond
            ((eq (variable-enumerated-domain x) t)
             (local
               (cond
                 ((member t (variable-enumerated-antidomain x) :test #'eq)
                  (cond ((member nil (variable-enumerated-antidomain x) :test #'eq)
                         (fail))
                        (t (setf (variable-enumerated-domain x) '(nil))
                           (setf (variable-enumerated-antidomain x) '())
                           (setf (variable-value x) nil))))
                 ((member nil (variable-enumerated-antidomain x) :test #'eq)
                  (setf (variable-enumerated-domain x) '(t))
                  (setf (variable-enumerated-antidomain x) '())
                  (setf (variable-value x) t))
                 (t (setf (variable-enumerated-domain x) '(t nil))
                    (unless (null (variable-enumerated-antidomain x))
                      (setf (variable-enumerated-antidomain x) '()))))))
            ((not (every #'booleanp (variable-enumerated-domain x)))
             ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
             ;;       This would also allow checking list only once.
             (set-enumerated-domain!
              x (remove-if-not #'booleanp (variable-enumerated-domain x)))))
          (run-noticers x)))))

(defun restrict-nonboolean! (x)
  ;; NOTE: X must be a variable.
  (unless (or (variable-possibly-integer? x)
              (variable-possibly-noninteger-real? x)
              (variable-possibly-nonreal-number? x)
              (variable-possibly-nonboolean-nonnumber? x))
    (fail))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (variable-possibly-boolean? x))
    (local (setf (variable-possibly-boolean? x) nil))
    (cond ((eq (variable-enumerated-domain x) t)
           (local (setf (variable-enumerated-antidomain x)
                        (adjoin t
                                (adjoin nil (variable-enumerated-antidomain x)
                                        :test #'eq)
                                :test #'eq))))
          ((some #'booleanp (variable-enumerated-domain x))
           ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
           ;;       This would also allow checking list only once.
           (set-enumerated-domain!
            x (remove-if #'booleanp (variable-enumerated-domain x)))))
    (run-noticers x)))

(defun restrict-lower-bound! (x lower-bound)
  ;; NOTE: X must be a variable.
  ;; NOTE: LOWER-BOUND must be a real constant.
  (if (variable-integer? x) (setf lower-bound (ceiling lower-bound)))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (or (not (variable-lower-bound x))
                 (> lower-bound (variable-lower-bound x))))
    (if (and (variable-upper-bound x) (< (variable-upper-bound x) lower-bound))
        (fail))
    (when (or (not (variable-lower-bound x))
              (not (variable-upper-bound x))
              (>= (/ (- lower-bound (variable-lower-bound x))
                     (- (variable-upper-bound x) (variable-lower-bound x)))
                  *minimum-shrink-ratio*))
      (local (setf (variable-lower-bound x) lower-bound))
      (cond ((eq (variable-enumerated-domain x) t)
             (if (and lower-bound
                      (variable-upper-bound x)
                      (variable-integer? x)
                      (or (null *maximum-discretization-range*)
                          (<= (- (variable-upper-bound x) lower-bound)
                              *maximum-discretization-range*)))
                 (set-enumerated-domain!
                  x (integers-between lower-bound
                                      (variable-upper-bound x)))))
            ((some #'(lambda (element) (< element lower-bound))
                   (variable-enumerated-domain x))
             ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
             ;;       This would also allow checking list only once.
             (set-enumerated-domain!
              x (remove-if #'(lambda (element) (< element lower-bound))
                           (variable-enumerated-domain x)))))
      (when (and (variable-lower-bound x)
                 (variable-upper-bound x)
                 (roughly-= (variable-upper-bound x) (variable-lower-bound x)))
        (local (setf (variable-value x) (variable-lower-bound x))))
      (run-noticers x))))

(defun restrict-upper-bound! (x upper-bound)
  ;; NOTE: X must be a variable.
  ;; NOTE: UPPER-BOUND must be a real constant.
  (when (variable-integer? x)
    (local (setf upper-bound (floor upper-bound))))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (or (not (variable-upper-bound x))
                 (< upper-bound (variable-upper-bound x))))
    (when (and (variable-lower-bound x) (> (variable-lower-bound x) upper-bound))
      (fail))
    (when (or (not (variable-lower-bound x))
              (not (variable-upper-bound x))
              (>= (/ (- (variable-upper-bound x) upper-bound)
                     (- (variable-upper-bound x) (variable-lower-bound x)))
                  *minimum-shrink-ratio*))
      (local (setf (variable-upper-bound x) upper-bound))
      (cond ((eq (variable-enumerated-domain x) t)
             (when (and (variable-lower-bound x)
                        upper-bound
                        (variable-integer? x)
                        (or (null *maximum-discretization-range*)
                            (<= (- upper-bound (variable-lower-bound x))
                                *maximum-discretization-range*)))
               (set-enumerated-domain!
                x (integers-between (variable-lower-bound x)
                                    upper-bound))))
            ((some #'(lambda (element) (> element upper-bound))
                   (variable-enumerated-domain x))
             ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
             ;;       This would also allow checking list only once.
             (set-enumerated-domain!
              x (remove-if #'(lambda (element) (> element upper-bound))
                           (variable-enumerated-domain x)))))
      (when (and (variable-lower-bound x)
                 (variable-upper-bound x)
                 (roughly-= (variable-lower-bound x) (variable-upper-bound x)))
        (local (setf (variable-value x) (variable-lower-bound x))))
      (run-noticers x))))

(defun restrict-bounds! (x lower-bound upper-bound)
  ;; NOTE: X must be a variable.
  ;; NOTE: LOWER-BOUND and UPPER-BOUND must be real constants.
  (when (variable-integer? x)
    (if lower-bound (setf lower-bound (ceiling lower-bound)))
    (if upper-bound (setf upper-bound (floor upper-bound))))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (and lower-bound
                   (or (not (variable-lower-bound x))
                       (> lower-bound (variable-lower-bound x))))
          (when (and (variable-upper-bound x)
                     (< (variable-upper-bound x) lower-bound))
            (fail))
          (when (or (not (variable-lower-bound x))
                    (not (variable-upper-bound x))
                    (>= (/ (- lower-bound (variable-lower-bound x))
                           (- (variable-upper-bound x) (variable-lower-bound x)))
                        *minimum-shrink-ratio*))
            (local (setf (variable-lower-bound x) lower-bound))
            (setf run? t)))
        (when (and upper-bound
                   (or (not (variable-upper-bound x))
                       (< upper-bound (variable-upper-bound x))))
          (when (and (variable-lower-bound x)
                     (> (variable-lower-bound x) upper-bound))
            (fail))
          (when (or (not (variable-lower-bound x))
                    (not (variable-upper-bound x))
                    (>= (/ (- (variable-upper-bound x) upper-bound)
                           (- (variable-upper-bound x) (variable-lower-bound x)))
                        *minimum-shrink-ratio*))
            (local (setf (variable-upper-bound x) upper-bound))
            (setf run? t)))
        (when run?
          (cond ((eq (variable-enumerated-domain x) t)
                 (if (and (variable-lower-bound x)
                          (variable-upper-bound x)
                          (variable-integer? x)
                          (or (null *maximum-discretization-range*)
                              (<= (- (variable-upper-bound x)
                                     (variable-lower-bound x))
                                  *maximum-discretization-range*)))
                     (set-enumerated-domain!
                      x (integers-between
                         (variable-lower-bound x)
                         (variable-upper-bound x)))))
                ((or (and lower-bound
                          (some #'(lambda (element) (< element lower-bound))
                                (variable-enumerated-domain x)))
                     (and upper-bound
                          (some #'(lambda (element) (> element upper-bound))
                                (variable-enumerated-domain x))))
                 ;; NOTE: Could do less consing if had LOCAL DELETE-IF.
                 ;;       This would also allow checking list only once.
                 (set-enumerated-domain!
                  x (remove-if #'(lambda (element)
                                   (or (and lower-bound (< element lower-bound))
                                       (and upper-bound (> element upper-bound))))
                               (variable-enumerated-domain x)))))
          ;; When the range-size of x is 0, set (variable-value x)
          (let ((domain (domain-size x))
                (range (range-size x))
                (enumerated (variable-enumerated-domain x))
                (lower (variable-lower-bound x)))
            (declare (type (or null real) range)
                     (type (or null integer) domain))
            (when (or (and (numberp domain) (= domain 1))
                      (and (numberp range) (roughly-= range 0)))
              (restrict-value! x (cond
                                   ;; No action if X already has a known numerical value
                                   ((numberp (variable-value x)) (variable-value x))
                                   ;; Use enumerated domain if it only has one element
                                   ((and (listp enumerated)
                                         ;; Enumerated domain has exactly one element
                                         enumerated (null (rest enumerated)))
                                    (first enumerated))
                                   ;; Bounds are equivalent
                                   ((= lower (variable-upper-bound x)) lower)
                                   ;; Should never happen, but just in case
                                   (t
                                    (warn "restrict-bounds! bug: ~A has a single-value domain that cannot be resolved!" x)
                                    (variable-value x))))))
          (run-noticers x)))))

(defun prune-enumerated-domain (x &optional (enumerated-domain (variable-enumerated-domain x)))
  ;; Returns an enumerated domain from which elements what violate
  ;; restrictions on X have been removed.
  (remove-if-not (lambda (elt)
                   (cond ((numberp elt)
                          (if (realp elt)
                              (when (cond ((integerp elt)
                                           (variable-possibly-integer? x))
                                          (t
                                           (variable-possibly-noninteger-real? x)))
                                (let ((low (variable-lower-bound x))
                                      (high (variable-upper-bound x)))
                                  (cond ((and low high)
                                         (<= low elt high))
                                        (low
                                         (<= low elt))
                                        (high
                                         (<= elt high))
                                        (t t))))
                              (variable-possibly-nonreal-number? x)))
                         ((booleanp elt)
                          (variable-possibly-boolean? x))
                         (t
                          (variable-possibly-nonboolean-nonnumber? x))))
                 enumerated-domain))

(defun share! (x y)
  ;; NOTE: X and Y must be variables such that (EQ X (VALUE-OF X)) and
  ;;       (EQ Y (VALUE-OF Y)).
  (let ((run? nil)
        (y-lower-bound? nil)
        (y-upper-bound? nil)
        (x-lower-bound (variable-lower-bound x))
        (x-upper-bound (variable-upper-bound x))
        (y-lower-bound (variable-lower-bound y))
        (y-upper-bound (variable-upper-bound y)))
    ;; Apply all restrictions from X to Y.
    (cond ((and (variable-integer? y) (not (variable-integer? x)))
           (if x-lower-bound (setf x-lower-bound (ceiling x-lower-bound)))
           (if x-upper-bound (setf x-upper-bound (floor x-upper-bound))))
          ((and (not (variable-integer? y)) (variable-integer? x))
           (when (and y-lower-bound (not (integerp y-lower-bound)))
             (setf y-lower-bound (ceiling y-lower-bound))
             (setf y-lower-bound? t))
           (when (and y-upper-bound (not (integerp y-upper-bound)))
             (setf y-upper-bound (floor y-upper-bound))
             (setf y-upper-bound? t))))
    (when (and (not (variable-possibly-integer? x))
               (variable-possibly-integer? y))
      (local (setf (variable-possibly-integer? y) nil))
      (setf run? t))
    (when (and (not (variable-possibly-noninteger-real? x))
               (variable-possibly-noninteger-real? y))
      (local (setf (variable-possibly-noninteger-real? y) nil))
      (setf run? t))
    (when (and (not (variable-possibly-nonreal-number? x))
               (variable-possibly-nonreal-number? y))
      (local (setf (variable-possibly-nonreal-number? y) nil))
      (setf run? t))
    (when (and (not (variable-possibly-boolean? x))
               (variable-possibly-boolean? y))
      (local (setf (variable-possibly-boolean? y) nil))
      (setf run? t))
    (when (and (not (variable-possibly-nonboolean-nonnumber? x))
               (variable-possibly-nonboolean-nonnumber? y))
      (local (setf (variable-possibly-nonboolean-nonnumber? y) nil))
      (setf run? t))
    (unless (or (variable-possibly-integer? y)
                (variable-possibly-noninteger-real? y)
                (variable-possibly-nonreal-number? y)
                (variable-possibly-boolean? y)
                (variable-possibly-nonboolean-nonnumber? y))
      (fail))
    (cond ((and x-lower-bound
                (or (not y-lower-bound) (> x-lower-bound y-lower-bound)))
           (local (setf (variable-lower-bound y) x-lower-bound))
           (setf run? t))
          (y-lower-bound?
           (local (setf (variable-lower-bound y) y-lower-bound))
           (setf run? t)))
    (cond ((and x-upper-bound
                (or (not y-upper-bound) (< x-upper-bound y-upper-bound)))
           (local (setf (variable-upper-bound y) x-upper-bound))
           (setf run? t))
          (y-upper-bound?
           (local (setf (variable-upper-bound y) y-upper-bound))
           (setf run? t)))
    (unless (or (null (variable-lower-bound y))
                (null (variable-upper-bound y))
                (< (variable-lower-bound y) (variable-upper-bound y)))
      (fail))
    (when run?
      ;; Something has changed: update enumerated domain of Y.
      (let ((lower-bound (variable-lower-bound y))
            (upper-bound (variable-upper-bound y)))
        (if (eq (variable-enumerated-domain y) t)
            (if (and lower-bound
                     upper-bound
                     (variable-integer? y)
                     (or (null *maximum-discretization-range*)
                         (<= (- upper-bound lower-bound)
                             *maximum-discretization-range*)))
                (set-enumerated-domain!
                 y (integers-between lower-bound upper-bound)))
            (set-enumerated-domain!
             y (prune-enumerated-domain y (variable-enumerated-domain y))))))
    (local (let* ((enumerated-domain
                    (cond
                      ((eq (variable-enumerated-domain x) t)
                       (if (eq (variable-enumerated-domain y) t)
                           t
                           (set-difference (variable-enumerated-domain y)
                                           (variable-enumerated-antidomain x)
                                           :test #'equal)))
                      ((eq (variable-enumerated-domain y) t)
                       (set-difference (variable-enumerated-domain x)
                                       (variable-enumerated-antidomain y)
                                       :test #'equal))
                      (t (intersection (variable-enumerated-domain x)
                                       (variable-enumerated-domain y)
                                       :test #'equal))))
                  (enumerated-antidomain
                    (if (eq enumerated-domain t)
                        (union (variable-enumerated-antidomain x)
                               (variable-enumerated-antidomain y)
                               :test #'equal)
                        '())))
             (if (null enumerated-domain) (fail))
             (if (and (not (eq enumerated-domain t))
                      (or (eq (variable-enumerated-domain y) t)
                          (< (length enumerated-domain)
                             (length (variable-enumerated-domain y)))))
                 (set-enumerated-domain!
                  y (prune-enumerated-domain y enumerated-domain)))
             (if (if (eq enumerated-domain t)
                     (> (length enumerated-antidomain)
                        (length (variable-enumerated-antidomain y)))
                     (not (null (variable-enumerated-antidomain y))))
                 (setf (variable-enumerated-antidomain y) enumerated-antidomain)))
      (setf (variable-noticers y)
            (append (variable-noticers y) (variable-noticers x)))
      (setf (variable-noticers x) '())
      (setf (variable-value x) y))
    (run-noticers y)))

(defun restrict-value! (x value)
  ;; NOTE: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; NOTE: VALUE must not be a variable.
  (if (occurs-in? x value) (fail))
  (etypecase value
    (integer (unless (variable-possibly-integer? x) (fail)))
    (real (unless (variable-possibly-noninteger-real? x) (fail)))
    (number (unless (variable-possibly-nonreal-number? x) (fail)))
    (boolean (unless (variable-possibly-boolean? x) (fail)))
    (t (unless (variable-possibly-nonboolean-nonnumber? x) (fail))))
  ;; needs work: This is sound only if VALUE does not contain any variables.
  (if (eq (variable-enumerated-domain x) t)
      (if (member value (variable-enumerated-antidomain x) :test #'equal)
          (fail))
      (unless (member value (variable-enumerated-domain x) :test #'equal)
        (fail)))
  (if (and (realp value)
           (or (and (variable-lower-bound x)
                    (< value (variable-lower-bound x)))
               (and (variable-upper-bound x)
                    (> value (variable-upper-bound x)))))
      (fail))
  (local (setf (variable-value x) value)
    (etypecase value
      (integer (if (variable-possibly-noninteger-real? x)
                   (setf (variable-possibly-noninteger-real? x) nil))
       (if (variable-possibly-nonreal-number? x)
           (setf (variable-possibly-nonreal-number? x) nil))
       (if (variable-possibly-boolean? x)
           (setf (variable-possibly-boolean? x) nil))
       (if (variable-possibly-nonboolean-nonnumber? x)
           (setf (variable-possibly-nonboolean-nonnumber? x) nil))
       (if (or (null (variable-lower-bound x))
               (not (integerp (variable-lower-bound x)))
               (> value (variable-lower-bound x)))
           (setf (variable-lower-bound x) value))
       (if (or (null (variable-upper-bound x))
               (not (integerp (variable-upper-bound x)))
               (< value (variable-upper-bound x)))
           (setf (variable-upper-bound x) value)))
      (real (if (variable-possibly-integer? x)
                (setf (variable-possibly-integer? x) nil))
       (if (variable-possibly-nonreal-number? x)
           (setf (variable-possibly-nonreal-number? x) nil))
       (if (variable-possibly-boolean? x)
           (setf (variable-possibly-boolean? x) nil))
       (if (variable-possibly-nonboolean-nonnumber? x)
           (setf (variable-possibly-nonboolean-nonnumber? x) nil))
       (if (or (null (variable-lower-bound x))
               (> value (variable-lower-bound x)))
           (setf (variable-lower-bound x) value))
       (if (or (null (variable-upper-bound x))
               (< value (variable-upper-bound x)))
           (setf (variable-upper-bound x) value)))
      (number (if (variable-possibly-integer? x)
                  (setf (variable-possibly-integer? x) nil))
       (if (variable-possibly-noninteger-real? x)
           (setf (variable-possibly-noninteger-real? x) nil))
       (if (variable-possibly-boolean? x)
           (setf (variable-possibly-boolean? x) nil))
       (if (variable-possibly-nonboolean-nonnumber? x)
           (setf (variable-possibly-nonboolean-nonnumber? x) nil)))
      (boolean (if (variable-possibly-integer? x)
                   (setf (variable-possibly-integer? x) nil))
       (if (variable-possibly-noninteger-real? x)
           (setf (variable-possibly-noninteger-real? x) nil))
       (if (variable-possibly-nonreal-number? x)
           (setf (variable-possibly-nonreal-number? x) nil))
       (if (variable-possibly-nonboolean-nonnumber? x)
           (setf (variable-possibly-nonboolean-nonnumber? x) nil)))
      (t (if (variable-possibly-integer? x)
             (setf (variable-possibly-integer? x) nil))
       (if (variable-possibly-noninteger-real? x)
           (setf (variable-possibly-noninteger-real? x) nil))
       (if (variable-possibly-nonreal-number? x)
           (setf (variable-possibly-nonreal-number? x) nil))
       (if (variable-possibly-boolean? x)
           (setf (variable-possibly-boolean? x) nil))))
    (cond ((eq (variable-enumerated-domain x) t)
           ;; needs work: This is sound only if VALUE does not contain any
           ;;             variables.
           (setf (variable-enumerated-domain x) (list value))
           (setf (variable-enumerated-antidomain x) '()))
          ((not (null (rest (variable-enumerated-domain x))))
           ;; needs work: This is sound only if VALUE does not contain any
           ;;             variables.
           (setf (variable-enumerated-domain x) (list value)))))
  (run-noticers x))

(defun restrict-true! (x)
  ;; NOTE: X must be a Boolean variable.
  (if (eq (variable-value x) nil) (fail))
  (when (eq (variable-value x) x)
    (local
      (setf (variable-value x) t)
      (setf (variable-enumerated-domain x) '(t)))
    (run-noticers x)))

(defun restrict-false! (x)
  ;; NOTE: X must be a Boolean variable.
  (if (eq (variable-value x) t) (fail))
  (when (eq (variable-value x) x)
    (local
      (setf (variable-value x) nil)
      (setf (variable-enumerated-domain x) '(nil)))
    (run-noticers x)))

(defun set-enumerated-domain! (x enumerated-domain)
  ;; NOTE: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; NOTE: All callers must insure that the new ENUMERATED-DOMAIN is a subset
  ;;       of the old one.
  ;; NOTE: Returns `nil' if domain isn't updated, `t' if it is
  (if (null enumerated-domain) (fail))
  (local
    (when
        ;; Update enumerated domain if it needs to be updated
        (cond
          ((eq (variable-enumerated-domain x) t)
           (setf (variable-enumerated-domain x) enumerated-domain)
           (unless (null (variable-enumerated-antidomain x))
             (setf (variable-enumerated-antidomain x) '()))
           (if (and (variable-possibly-boolean? x)
                    (not (some #'booleanp enumerated-domain)))
               (setf (variable-possibly-boolean? x) nil))
           (if (and (variable-possibly-nonboolean-nonnumber? x)
                    (not (some #'(lambda (x)
                                   (and (not (booleanp x)) (not (numberp x))))
                               enumerated-domain)))
               (setf (variable-possibly-nonboolean-nonnumber? x) nil))
           (if (and (variable-possibly-nonreal-number? x)
                    (not (some #'(lambda (x) (and (not (realp x)) (numberp x)))
                               enumerated-domain)))
               (setf (variable-possibly-nonreal-number? x) nil))
           (if (and (variable-possibly-noninteger-real? x)
                    (not (some #'(lambda (x) (and (not (integerp x)) (realp x)))
                               enumerated-domain)))
               (setf (variable-possibly-noninteger-real? x) nil))
           (if (and (variable-possibly-integer? x)
                    (not (some #'integerp enumerated-domain)))
               (setf (variable-possibly-integer? x) nil))
           (if (variable-real? x)
               (let ((lower-bound (reduce #'min enumerated-domain))
                     (upper-bound (reduce #'max enumerated-domain)))
                 (if (or (null (variable-lower-bound x))
                         (> lower-bound (variable-lower-bound x)))
                     (setf (variable-lower-bound x) lower-bound))
                 (if (or (null (variable-upper-bound x))
                         (< upper-bound (variable-upper-bound x)))
                     (setf (variable-upper-bound x) upper-bound))))
           (if (null (rest enumerated-domain))
               (setf (variable-value x) (first enumerated-domain)))
           t)
          ((< (length enumerated-domain) (length (variable-enumerated-domain x)))
           (setf (variable-enumerated-domain x) enumerated-domain)
           (if (and (variable-possibly-boolean? x)
                    (not (some #'booleanp enumerated-domain)))
               (setf (variable-possibly-boolean? x) nil))
           (if (and (variable-possibly-nonboolean-nonnumber? x)
                    (not (some #'(lambda (x)
                                   (and (not (booleanp x)) (not (numberp x))))
                               enumerated-domain)))
               (setf (variable-possibly-nonboolean-nonnumber? x) nil))
           (if (and (variable-possibly-nonreal-number? x)
                    (not (some #'(lambda (x) (and (not (realp x)) (numberp x)))
                               enumerated-domain)))
               (setf (variable-possibly-nonreal-number? x) nil))
           (if (and (variable-possibly-noninteger-real? x)
                    (not (some #'(lambda (x) (and (not (integerp x)) (realp x)))
                               enumerated-domain)))
               (setf (variable-possibly-noninteger-real? x) nil))
           (if (and (variable-possibly-integer? x)
                    (not (some #'integerp enumerated-domain)))
               (setf (variable-possibly-integer? x) nil))
           (if (variable-real? x)
               (let ((lower-bound (reduce #'min enumerated-domain))
                     (upper-bound (reduce #'max enumerated-domain)))
                 (if (or (null (variable-lower-bound x))
                         (> lower-bound (variable-lower-bound x)))
                     (setf (variable-lower-bound x) lower-bound))
                 (if (or (null (variable-upper-bound x))
                         (< upper-bound (variable-upper-bound x)))
                     (setf (variable-upper-bound x) upper-bound))))
           (if (null (rest enumerated-domain))
               (setf (variable-value x) (first enumerated-domain)))
           t))
      ;; Enumerated domain was updated, so
      ;; update `variable-type' of x
      (setf (variable-type x)
            ;; Annotate the type with the current enumerated domain
            (serapeum:~>>
             ;; Get the current variable type
             (variable-type x)
             ;; Remove prior enumeration types
             ;; (e.g. from a prior run of
             ;; `set-enumerated-domain!')
             (remove-if
              (serapeum:op
                (match _
                  ((guard
                    (list* 'or args)
                    ;; Every component of args is a `value'
                    ;; form.
                    (every (lambda-match ((list 'value _) t)) args))
                   t))))
             ;; Add the new domain
             (cons (variable-enumerated-domain-type x))))
      ;; Return t to signal success
      t)))

(defun restrict-enumerated-domain! (x enumerated-domain)
  ;; NOTE: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; NOTE: ENUMERATED-DOMAIN must not be a variable.
  (unless (typep enumerated-domain 'sequence) (fail))
  (when (every #'ground? enumerated-domain)
    (setf enumerated-domain
          (remove-duplicates (map 'list #'eliminate-variables enumerated-domain)
                             :test #'equal))
    (unless (variable-possibly-boolean? x)
      (setf enumerated-domain (remove-if #'booleanp enumerated-domain)))
    (unless (variable-possibly-nonboolean-nonnumber? x)
      (setf enumerated-domain
            (remove-if #'(lambda (x) (and (not (booleanp x)) (not (numberp x))))
                       enumerated-domain)))
    (unless (variable-possibly-nonreal-number? x)
      (setf enumerated-domain
            (remove-if #'(lambda (x) (and (not (realp x)) (numberp x)))
                       enumerated-domain)))
    (unless (variable-possibly-noninteger-real? x)
      (setf enumerated-domain
            (remove-if #'(lambda (x) (and (not (integerp x)) (realp x)))
                       enumerated-domain)))
    (unless (variable-possibly-integer? x)
      (setf enumerated-domain (remove-if #'integerp enumerated-domain)))
    (if (variable-upper-bound x)
        (let ((upper-bound (variable-upper-bound x)))
          (setf enumerated-domain
                (remove-if #'(lambda (element) (> element upper-bound))
                           enumerated-domain))))
    (if (variable-lower-bound x)
        (let ((lower-bound (variable-lower-bound x)))
          (setf enumerated-domain
                (remove-if #'(lambda (element) (< element lower-bound))
                           enumerated-domain))))
    (setf enumerated-domain
          (if (eq (variable-enumerated-domain x) t)
              (set-difference enumerated-domain
                              (variable-enumerated-antidomain x)
                              :test #'equal)
              (intersection (variable-enumerated-domain x) enumerated-domain
                            :test #'equal)))
    (when (set-enumerated-domain! x enumerated-domain)
      (run-noticers x))))

(defun restrict-enumerated-antidomain! (x enumerated-antidomain)
  ;; NOTE: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; NOTE: ENUMERATED-ANTIDOMAIN must not be a variable.
  (unless (typep enumerated-antidomain 'sequence) (fail))
  (when (every #'ground? enumerated-antidomain)
    (setf enumerated-antidomain
          (remove-duplicates
           (map 'list #'eliminate-variables enumerated-antidomain)
           :test #'equal))
    (cond
      ((eq (variable-enumerated-domain x) t)
       (setf enumerated-antidomain
             (union (variable-enumerated-antidomain x) enumerated-antidomain
                    :test #'equal))
       (when (> (length enumerated-antidomain)
                (length (variable-enumerated-antidomain x)))
         (local (setf (variable-enumerated-antidomain x) enumerated-antidomain))
         (run-noticers x)))
      ((set-enumerated-domain!
        x (set-difference (variable-enumerated-domain x) enumerated-antidomain
                          :test #'equal))
       (run-noticers x)))))

;;; Rules

(defun +-rule-up (z x y)
  (when (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
  ;; NOTE: We can't assert that Z in not an integer when either X or Y are not
  ;;       integers since they may be Gaussian integers. But we can if either
  ;;       X or Y is real. If the Screamer type system could distinguish
  ;;       Gaussian integers from other complex numbers we could whenever X or
  ;;       Y was not a Gaussian integer.
  ;; NOTE: Classic Screamer assumed that an integer can only be added to
  ;; by two other integers. This is false, for instance 1 = 1/3 + 2/3.
  ;; (when (and (or (variable-noninteger? x) (variable-noninteger? y))
  ;;            (or (variable-real? x) (variable-real? y)))
  ;;   (restrict-noninteger! z))
  ;; NOTE: To mitigate the loss of the above, we check if one of X and Y are
  ;; integers and the other isn't. In this case, the other must be a non-integer
  ;; rational (in which case Z is a non-integer rational) or a non-rational real
  ;; (e.g. floats) (in which case Z cannot be an integer or rational)
  (when (and (alexandria:xor (variable-noninteger? x) (variable-noninteger? y))
             ;; We assume both are reals. This should be guaranteed
             ;; by the callers, but it's good and cheap to verify
             (variable-real? x) (variable-real? y)
             (or (variable-integer? x) (variable-integer? y)))
    (restrict-noninteger! z))
  ;; NOTE: To mitigate the loss of the above, when X and Y have explicit
  ;; enumerated domains we brute force whether Z must be a non-integer
  ;; TODO: Tracking rationals explicitly as a superset of integers
  ;; would resolve this case much more simply
  (when (or
         ;; If Z has an enumerated domain which doesn't have integers
         (and (listp (variable-enumerated-domain z))
              (not (some #'integerp (variable-enumerated-domain z))))
         ;; Check if X+Y must be a non-integer real
         (and (variable-real? x) (variable-real? y)
              ;; No point checking unless we already know Z could be
              ;; a non-integer real
              (variable-possibly-noninteger-real? z)
              ;; No point checking if we already know Z is a
              ;; non-integer
              (not (variable-noninteger? z))
              ;; Only check when both have explicit enumerated domains
              (listp (variable-enumerated-domain x))
              (listp (variable-enumerated-domain y))
              ;; Check if X+Y must not be an integer
              (let ((integer-possible nil)
                    ;; Remove non-rationals since they can't resolve to integers
                    ;; NOTE: If Screamer could track Gaussian integers, this
                    ;; would be an invalid optimization, as e.g. floats could
                    ;; also produce Gaussian integers
                    (x-dom (remove-if-not #'rationalp (variable-enumerated-domain x)))
                    (y-dom (remove-if-not #'rationalp (variable-enumerated-domain y))))
                (block +-rule-up-check-noninteger-by-domain
                  ;; Iterate through the enumerated domains
                  (dolist (x-value x-dom)
                    (dolist (y-value y-dom)
                      (when (integerp (+ x-value y-value))
                        (setf integer-possible t)
                        (return-from +-rule-up-check-noninteger-by-domain)))))
                ;; Integer is not possible
                (not integer-possible))))
    (restrict-noninteger! z))
  (when (and (variable-real? x) (variable-real? y)) (restrict-real! z))
  ;; NOTE: Reals can only be produced by addition of reals
  (when (and (or (variable-nonreal? x) (variable-nonreal? y))
             (or (variable-real? x) (variable-real? y)))
    (restrict-nonreal! z))
  (when (and (variable-real? x) (variable-real? y) (variable-real? z))
    (let ((old-bounds (list (variable-lower-bound z)
                            (variable-upper-bound z))))
      (restrict-bounds!
       z
       (infinity-+ (variable-lower-bound x) (variable-lower-bound y))
       (infinity-+ (variable-upper-bound x) (variable-upper-bound y)))
      ;; Propagate new bounds
      (unless (equal old-bounds
                     (list (variable-lower-bound z)
                           (variable-upper-bound z)))
        (run-noticers z))))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (when (and (not (variable? x))
               (not (variable? y))
               (not (variable? z))
               (/= z (+ x y)))
      (fail))))

(defun +-rule-down (z x y)
  ;; NOTE: We can't assert that X and Y are integers when Z is an integer since
  ;;       Z may be an integer when X and Y are Gaussian integers. But we can
  ;;       make such an assertion if either X or Y is an integer. If the Screamer
  ;;       type system could distinguish Gaussian integers from other complex
  ;;       numbers we could make such an assertion whenever either X or Y was
  ;;       not a Gaussian integer.
  (when (and (variable-integer? z) (or (variable-integer? x) (variable-integer? y)))
    (restrict-integer! x))
  ;; NOTE: Reals can only be produced by addition of reals
  (when (and (variable-real? z) (or (variable-real? x) (variable-real? y)))
    (restrict-real! x))
  (when (and (variable-real? x) (variable-real? y) (variable-real? z))
    (let ((old-bounds (list (variable-lower-bound x)
                            (variable-upper-bound x))))
      (restrict-bounds!
       x
       (infinity-- (variable-lower-bound z) (variable-upper-bound y))
       (infinity-- (variable-upper-bound z) (variable-lower-bound y)))
      ;; Propagate new bounds
      (unless (equal old-bounds
                     (list (variable-lower-bound x)
                           (variable-upper-bound x)))
        (run-noticers x))))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? x))
             (not (variable? y))
             (not (variable? z))
             (/= z (+ x y)))
        (fail))))

(defun /-rule (z x y)
  (when (and (variable-lower-bound x) (plusp (variable-lower-bound x)))
    (cond ((and (variable-upper-bound x) (not (zerop (variable-upper-bound x))))
           (if (variable-lower-bound z)
               (cond
                 ((minusp (variable-lower-bound z))
                  (restrict-lower-bound!
                   y (/ (variable-lower-bound z) (variable-lower-bound x))))
                 (t (restrict-lower-bound! y 0)
                    (restrict-lower-bound!
                     y (/ (variable-lower-bound z) (variable-upper-bound x))))))
           (if (variable-upper-bound z)
               (cond
                 ((plusp (variable-upper-bound z))
                  (restrict-upper-bound!
                   y (/ (variable-upper-bound z) (variable-lower-bound x))))
                 (t (restrict-upper-bound! y 0)
                    (restrict-upper-bound!
                     y (/ (variable-upper-bound z) (variable-upper-bound x)))))))
          (t (if (variable-lower-bound z)
                 (cond
                   ((minusp (variable-lower-bound z))
                    (restrict-lower-bound!
                     y (/ (variable-lower-bound z) (variable-lower-bound x))))
                   (t (restrict-lower-bound! y 0))))
             (if (variable-upper-bound z)
                 (cond
                   ((plusp (variable-upper-bound z))
                    (restrict-upper-bound!
                     y (/ (variable-upper-bound z) (variable-lower-bound x))))
                   (t (restrict-upper-bound! y 0)))))))
  (when (and (variable-upper-bound x) (minusp (variable-upper-bound x)))
    (cond ((and (variable-lower-bound x) (not (zerop (variable-lower-bound x))))
           (if (variable-upper-bound z)
               (cond
                 ((plusp (variable-upper-bound z))
                  (restrict-lower-bound!
                   y (/ (variable-upper-bound z) (variable-upper-bound x))))
                 (t (restrict-lower-bound! y 0)
                    (restrict-lower-bound!
                     y (/ (variable-upper-bound z) (variable-lower-bound x))))))
           (if (variable-lower-bound z)
               (cond
                 ((minusp (variable-lower-bound z))
                  (restrict-upper-bound!
                   y (/ (variable-lower-bound z) (variable-upper-bound x))))
                 (t (restrict-upper-bound! y 0)
                    (restrict-upper-bound!
                     y (/ (variable-lower-bound z) (variable-lower-bound x)))))))
          (t (if (variable-upper-bound z)
                 (cond
                   ((plusp (variable-upper-bound z))
                    (restrict-lower-bound!
                     y (/ (variable-upper-bound z) (variable-upper-bound x))))
                   (t (restrict-lower-bound! y 0))))
             (if (variable-lower-bound z)
                 (cond
                   ((minusp (variable-lower-bound z))
                    (restrict-upper-bound!
                     y (/ (variable-lower-bound z) (variable-upper-bound x))))
                   (t (restrict-upper-bound! y 0))))))))

(defun *-rule-up (z x y)
  (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
  ;; NOTE: We can't assert that Z in not an integer when either X or Y are not
  ;;       integers since they may be Gaussian integers. But we can if either
  ;;       X or Y is real. If the Screamer type system could distinguish
  ;;       Gaussian integers from other complex numbers we could whenever X or
  ;;       Y was not a Gaussian integer.
  ;;
  ;; NOTE: Consider for instance 2/3 * 3/2. This produces an integer output.
  ;; As such, we cannot assume that non-integers will not produce an integer,
  ;; as classic Screamer assumed..
  ;; (if (and (or (variable-noninteger? x) (variable-noninteger? y))
  ;;          (or (variable-real? x) (variable-real? y)))
  ;;     (restrict-noninteger! z))
  ;; NOTE: To minimize the effect of removing the above assumption, we
  ;; check if X and Y have enumerated domains, and if so check the full
  ;; domain to see if Z is a non-integer.
  ;; TODO: Tracking rationals explicitly as a superset of integers
  ;; would resolve this case much more simply
  (when (or
         ;; If Z has an enumerated domain which doesn't have integers
         (and (listp (variable-enumerated-domain z))
              (not (some #'integerp (variable-enumerated-domain z))))
         ;; Check if X*Y must be a non-integer real
         (and (variable-real? x) (variable-real? y)
              ;; No point checking unless we already know Z could be
              ;; a non-integer real
              (variable-possibly-noninteger-real? z)
              ;; No point checking if we already know Z is a
              ;; non-integer
              (not (variable-noninteger? z))
              ;; Only check when both have explicit enumerated domains
              (listp (variable-enumerated-domain x))
              (listp (variable-enumerated-domain y))
              ;; Check if X*Y must not be an integer
              (let ((integer-possible nil)
                    ;; Remove non-rationals since they can't resolve to integers
                    ;; NOTE: If Screamer could track Gaussian integers, this
                    ;; would be an invalid optimization, as e.g. floats could
                    ;; also produce Gaussian integers
                    (x-dom (remove-if-not #'rationalp (variable-enumerated-domain x)))
                    (y-dom (remove-if-not #'rationalp (variable-enumerated-domain y))))
                (block *-rule-up-check-noninteger-by-domain
                  ;; Iterate through the enumerated domains
                  (dolist (x-value x-dom)
                    (dolist (y-value y-dom)
                      (when (integerp (* x-value y-value))
                        (setf integer-possible t)
                        (return-from *-rule-up-check-noninteger-by-domain)))))
                ;; Integer is not possible
                (not integer-possible))))
    (restrict-noninteger! z))
  (if (and (variable-real? x) (variable-real? y)) (restrict-real! z))
  ;; NOTE: If either X or Y is non-real and the other is real, Z must be non-real.
  (if (and (or (variable-nonreal? x) (variable-nonreal? y))
           (or (variable-real? x) (variable-real? y)))
      (restrict-nonreal! z))
  (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      ;; NOTE: Can sometimes do better than the following even when ranges are
      ;;       not finite.
      (restrict-bounds!
       z
       (s:nest
        (infinity-min
         (infinity-* (variable-lower-bound x)
                     (variable-lower-bound y)))
        (infinity-min
         (infinity-* (variable-lower-bound x)
                     (variable-upper-bound y)))
        (infinity-min
         (infinity-* (variable-upper-bound x)
                     (variable-lower-bound y))
         (infinity-* (variable-upper-bound x)
                     (variable-upper-bound y))))
       (s:nest
        (infinity-max
         (infinity-* (variable-lower-bound x)
                     (variable-lower-bound y)))
        (infinity-max
         (infinity-* (variable-lower-bound x)
                     (variable-upper-bound y)))
        (infinity-max
         (infinity-* (variable-upper-bound x)
                     (variable-lower-bound y))
         (infinity-* (variable-upper-bound x)
                     (variable-upper-bound y))))))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? x))
             (not (variable? y))
             (not (variable? z))
             (/= z (* x y)))
        (fail))))

(defun *-rule-down (z x y)
  ;; NOTE: We can't assert that X and Y are integers when Z is an integer since
  ;;       Z may be an integer when X and Y are Gaussian integers. But we can
  ;;       make such an assertion if either X or Y is an integer. If the Screamer
  ;;       type system could distinguish Gaussian integers from other complex
  ;;       numbers we could make such an assertion whenever either X or Y was
  ;;       not a Gaussian integer.
  ;; NOTE: Classic Screamer assumed that an integer can only be multiplied to
  ;; by two other integers. This is false, for instance 1 = 2 * 1/2.
  ;; (if (and (variable-integer? z) (or (variable-integer? x) (variable-integer? y)))
  ;;     (restrict-integer! x))
  ;; NOTE: To mitigate the loss of the above, when Z and Y have explicit
  ;; enumerated domains we brute force whether X must be an integer
  ;; TODO: Tracking rationals explicitly as a superset of integers
  ;; would resolve this case much more simply
  (when (and (variable-real? z) (variable-real? y)
             ;; No point checking if we already know it's not an integer
             (variable-possibly-integer? x)
             ;; No point checking if we know for sure X is an integer
             (not (variable-integer? x))
             ;; When both have explicit enumerated domains
             (listp (variable-enumerated-domain z))
             (listp (variable-enumerated-domain y))
             ;; Check if Z/Y must be an integer
             (let ((noninteger-possible nil)
                   ;; Remove non-rationals since they can't resolve to integers
                   ;; NOTE: If Screamer could track Gaussian integers, this
                   ;; would be an invalid optimization, as e.g. floats could
                   ;; also produce Gaussian integers
                   (z-dom (remove-if-not #'rationalp (variable-enumerated-domain z)))
                   (y-dom (remove-if-not #'rationalp (variable-enumerated-domain y))))
               (block *-rule-down-check-necessarily-integer-by-domain
                 ;; Iterate through the enumerated domains
                 (dolist (z-value z-dom)
                   (dolist (y-value y-dom)
                     (unless (or (zerop y-value) (integerp (/ z-value y-value)))
                       (setf noninteger-possible t)
                       (return-from *-rule-down-check-necessarily-integer-by-domain)))))
               (not noninteger-possible)))
    (restrict-integer! x))
  ;; NOTE: Reals can only be produced by multiplication of reals.
  (if (and (variable-real? z) (or (variable-real? x) (variable-real? y)))
      (restrict-real! x))
  (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      (/-rule z y x))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? x))
             (not (variable? y))
             (not (variable? z))
             (/= z (* x y)))
        (fail))))

(defun min-rule-up (z x y)
  (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
  (restrict-bounds!
   z
   (infinity-min (variable-lower-bound x) (variable-lower-bound y))
   (if (variable-upper-bound x)
       (if (variable-upper-bound y)
           (min (variable-upper-bound x) (variable-upper-bound y))
           (variable-upper-bound x))
       (variable-upper-bound y)))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? z))
             (not (variable? x))
             (not (variable? y))
             (/= z (min x y)))
        (fail))))

(defun min-rule-down (z x y)
  ;; NOTE: The analog of the following for upper bounds, namely restricting
  ;;       the upper bound of either X or Y to (VARIABLE-UPPER-BOUND Z) is
  ;;       nondeterministic.
  (if (variable-lower-bound z)
      (restrict-lower-bound! x (variable-lower-bound z)))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? z))
             (not (variable? x))
             (not (variable? y))
             (/= z (min x y)))
        (fail))))

(defun max-rule-up (z x y)
  (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
  (restrict-bounds!
   z
   (if (variable-lower-bound x)
       (if (variable-lower-bound y)
           (max (variable-lower-bound x) (variable-lower-bound y))
           (variable-lower-bound x))
       (variable-lower-bound y))
   (infinity-max (variable-upper-bound x) (variable-upper-bound y)))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? z))
             (not (variable? x))
             (not (variable? y))
             (/= z (max x y)))
        (fail))))

(defun max-rule-down (z x y)
  ;; NOTE: The analog of the following for lower bounds, namely restricting
  ;;       the lower bound of either X or Y to (VARIABLE-LOWER-BOUND Z) is
  ;;       nondeterministic.
  (if (variable-upper-bound z)
      (restrict-upper-bound! x (variable-upper-bound z)))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? z))
             (not (variable? x))
             (not (variable? y))
             (/= z (max x y)))
        (fail))))

(defun =-rule (x y)
  (cond
    ;; NOTE: I forget why +-RULE *-RULE MIN-RULE and MAX-RULE must perform the
    ;;       check in the second COND clause irrespective of whether the first
    ;;       clause is executed.
    ((and (variable-real? x) (variable-real? y))
     (restrict-bounds! x (variable-lower-bound y) (variable-upper-bound y))
     (restrict-bounds! y (variable-lower-bound x) (variable-upper-bound x)))
    ((and (not (variable? x)) (not (variable? y)) (/= x y)) (fail)))
  (when (or (variable? x) (variable? y))
    (let ((xdom (cond
                  ((and (variable? x)
                        (subtypep (type-of (variable-enumerated-domain x)) 'list))
                   (variable-enumerated-domain x))
                  ((bound? x) (list (value-of x)))
                  (t nil)))
          (ydom (cond
                  ((and (variable? y)
                        (subtypep (type-of (variable-enumerated-domain y)) 'list))
                   (variable-enumerated-domain y))
                  ((bound? y) (list (value-of y)))
                  (t nil))))
      (when (and xdom ydom)
        (let ((joined (intersection xdom ydom)))
          (mapc
           (lambda (v)
             (when (variable? v)
               (restrict-enumerated-domain! v joined)))
           (list x y)))))))

(defun <=-rule (x y)
  (if (variable-lower-bound x)
      (restrict-lower-bound! y (variable-lower-bound x)))
  (if (variable-upper-bound y)
      (restrict-upper-bound! x (variable-upper-bound y))))

(defun <-rule (x y)
  (if (variable-lower-bound x)
      (restrict-lower-bound! y (if (variable-integer? y)
                                   (1+ (floor (variable-lower-bound x)))
                                   (variable-lower-bound x))))
  (if (variable-upper-bound y)
      (restrict-upper-bound! x (if (variable-integer? x)
                                   (1- (ceiling (variable-upper-bound y)))
                                   (variable-upper-bound y))))
  (let ((x (value-of x))
        (y (value-of y)))
    (if (and (not (variable? x)) (not (variable? y)) (>= x y)) (fail))))

(defun /=-rule (x y)
  ;; NOTE: Got rid of the nondeterministic version of /=-RULE.
  (let ((xv (value-of x))
        (yv (value-of y)))
    (cond ((and (not (variable? xv)) (not (variable? yv)) (= xv yv)) (fail))
          ((and (bound? xv)
                (variable? y))
           (if (listp (variable-enumerated-domain y))
               (when (member (value-of xv) (variable-enumerated-domain y))
                 (restrict-enumerated-domain! y (remove (value-of xv) (variable-enumerated-domain y))))
               (restrict-enumerated-antidomain! y (cons (value-of xv) (variable-enumerated-antidomain y)))))
          ((and (bound? yv)
                (variable? x))
           (if (listp (variable-enumerated-domain x))
               (when (member (value-of yv) (variable-enumerated-domain x))
                 (restrict-enumerated-domain! x (remove (value-of yv) (variable-enumerated-domain x))))
               (restrict-enumerated-antidomain! x (cons (value-of yv) (variable-enumerated-antidomain x))))))))

;;; Lifted Arithmetic Functions (Two argument optimized)

(defun +v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  ;; needs work: The first two optimizations below violate Common Lisp type
  ;;             propagation conventions.
  (cond ((and (bound? x) (zerop (value-of x))) (value-of y))
        ((and (bound? y) (zerop (value-of y))) (value-of x))
        ((and (bound? x) (bound? y)) (+ (value-of x) (value-of y)))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-numberv)))
             (attach-noticer!
              #'(lambda () (+-rule-up z x y) (+-rule-down z y x)) x)
             (attach-noticer!
              #'(lambda () (+-rule-up z x y) (+-rule-down z x y)) y)
             (attach-noticer!
              #'(lambda () (+-rule-down z x y) (+-rule-down z y x)) z
              :dependencies (list x y))
             z))))

(defun -v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  ;; needs work: The first optimization below violates Common Lisp type
  ;;             propagation conventions.
  (cond ((and (bound? y) (zerop (value-of y))) (value-of x))
        ((and (bound? x) (bound? y)) (- (value-of x) (value-of y)))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-numberv)))
             (attach-noticer!
              #'(lambda () (+-rule-down x y z) (+-rule-down x z y)) x)
             (attach-noticer!
              #'(lambda () (+-rule-up x y z) (+-rule-down x z y)) y)
             (attach-noticer!
              #'(lambda () (+-rule-up x y z) (+-rule-down x y z)) z
              :dependencies (list x y))
             z))))

(defun *v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  ;; needs work: The first four optimizations below violate Common Lisp type
  ;;             propagation conventions.
  (cond ((and (bound? x) (zerop (value-of x))) 0)
        ((and (bound? y) (zerop (value-of y))) 0)
        ((and (bound? x) (= (value-of x) 1)) (value-of y))
        ((and (bound? y) (= (value-of y) 1)) (value-of x))
        ((and (bound? x) (bound? y)) (* (value-of x) (value-of y)))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-numberv)))
             (attach-noticer!
              #'(lambda () (*-rule-up z x y) (*-rule-down z y x)) x)
             (attach-noticer!
              #'(lambda () (*-rule-up z x y) (*-rule-down z x y)) y)
             (attach-noticer!
              #'(lambda () (*-rule-down z x y) (*-rule-down z y x)) z
              :dependencies (list x y))
             z))))

(defun /v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  ;; needs work: The first three optimizations below violate Common Lisp type
  ;;             propagation conventions.
  (cond ((and (bound? x) (zerop (value-of x))) 0)
        ((and (bound? y) (zerop (value-of y))) (fail))
        ((and (bound? y) (= (value-of y) 1)) (value-of x))
        ((and (bound? x) (bound? y)) (/ (value-of x) (value-of y)))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-numberv)))
             (attach-noticer!
              #'(lambda () (*-rule-down x y z) (*-rule-down x z y)) x)
             (attach-noticer!
              #'(lambda () (*-rule-up x y z) (*-rule-down x z y)) y)
             (attach-noticer!
              #'(lambda ()
                  ;; NOTE: We do not know whether the quotient Z
                  ;; will be part of the final `solution' form,
                  ;; so we cannot unilaterally force Y to be
                  ;; non-zero without breaking backwards-compatibility
                  ;; with classic Screamer.
                  ;; If Z *does* ever try to constrain Y, we can
                  ;; add this constraint as well.
                  (restrict-enumerated-antidomain! y '(0))
                  (*-rule-up x y z) (*-rule-down x y z)) z
              :dependencies (list x y))
             z))))

(defun minv2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (cond ((known?-<=v2-internal x y) (value-of x))
        ((known?-<=v2-internal y x) (value-of y))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-realv)))
             (attach-noticer!
              #'(lambda () (min-rule-up z x y) (min-rule-down z y x)) x)
             (attach-noticer!
              #'(lambda () (min-rule-up z x y) (min-rule-down z x y)) y)
             (attach-noticer!
              #'(lambda () (min-rule-down z x y) (min-rule-down z y x)) z
              :dependencies (list x y))
             z))))

(defun maxv2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (cond ((known?-<=v2-internal y x) (value-of x))
        ((known?-<=v2-internal x y) (value-of y))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-realv)))
             (attach-noticer!
              #'(lambda () (max-rule-up z x y) (max-rule-down z y x)) x)
             (attach-noticer!
              #'(lambda () (max-rule-up z x y) (max-rule-down z x y)) y)
             (attach-noticer!
              #'(lambda () (max-rule-down z x y) (max-rule-down z y x)) z
              :dependencies (list x y))
             z))))

;;; Lifted Type Functions (KNOWN? optimized)

(defun known?-integerpv (x)
  (let ((x (value-of x)))
    (etypecase x
      (integer t)
      (variable (variable-integer? x))
      (t nil))))

(defun known?-notv-integerpv (x)
  (let ((x (value-of x)))
    (etypecase x
      (integer nil)
      (variable (variable-noninteger? x))
      (t t))))

(defun known?-realpv (x)
  (let ((x (value-of x)))
    (etypecase x
      (real t)
      (variable (variable-real? x))
      (t nil))))

(defun known?-notv-realpv (x)
  (let ((x (value-of x)))
    (etypecase x
      (real nil)
      (variable (variable-nonreal? x))
      (t t))))

(defun known?-numberpv (x)
  (let ((x (value-of x)))
    (etypecase x
      (number t)
      (variable (variable-number? x))
      (t nil))))

(defun known?-notv-numberpv (x)
  (let ((x (value-of x)))
    (etypecase x
      (number nil)
      (variable (variable-nonnumber? x))
      (t t))))

(defun known?-booleanpv (x)
  (let ((x (value-of x)))
    (etypecase x
      (boolean t)
      (variable (variable-boolean? x))
      (t nil))))

(defun known?-notv-booleanpv (x)
  (let ((x (value-of x)))
    (etypecase x
      (boolean nil)
      (variable (variable-nonboolean? x))
      (t t))))

;;; Lifted Arithmetic Comparison Functions (Two argument KNOWN? optimized)

(defun known?-<=v2-variable (x y)
  (and (variable-upper-bound x)
       (variable-lower-bound y)
       (<= (variable-upper-bound x) (variable-lower-bound y))))

(defun known?-<v2-variable (x y)
  (and (variable-upper-bound x)
       (variable-lower-bound y)
       (< (variable-upper-bound x) (variable-lower-bound y))))

(defun known?-=v2-variable (x y)
  (or (and (variable-real? x)
           (variable-real? y)
           (known?-<=v2-variable x y)
           (known?-<=v2-variable y x))
      (and (not (eq x (variable-value x)))
           (not (eq y (variable-value y)))
           (= (variable-value x) (variable-value y)))))

(defun known?-/=v2-variable (x y)
  (or (and (variable-real? x)
           (variable-real? y)
           (or (known?-<v2-variable x y) (known?-<v2-variable y x)))
      (and (not (eq x (variable-value x)))
           (not (eq y (variable-value y)))
           (/= (variable-value x) (variable-value y)))))

(defun known?-=v2-internal (x y)
  (known?-=v2-variable (variablize x) (variablize y)))

(defun known?-<=v2-internal (x y)
  (known?-<=v2-variable (variablize x) (variablize y)))

(defun known?-<v2-internal (x y)
  (known?-<v2-variable (variablize x) (variablize y)))

(defun known?-/=v2-internal (x y)
  (known?-/=v2-variable (variablize x) (variablize y)))

(defun known?-=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (known?-=v2-internal x y))

(defun known?-<=v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (known?-<=v2-internal x y))

(defun known?-<v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (known?-<v2-internal x y))

(defun known?-/=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (known?-/=v2-internal x y))

;;; Lifted Type Functions (ASSERT! optimized)

(defun assert!-integerpv (x)
  (let ((x (value-of x)))
    (typecase x
      (integer)
      (variable (restrict-integer! x))
      (otherwise (fail)))))

(defun assert!-notv-integerpv (x)
  (let ((x (value-of x)))
    (typecase x
      (integer (fail))
      (variable (restrict-noninteger! x))
      (otherwise))))

(defun assert!-realpv (x)
  (let ((x (value-of x)))
    (typecase x
      (real)
      (variable (restrict-real! x))
      (otherwise (fail)))))

(defun assert!-notv-realpv (x)
  (let ((x (value-of x)))
    (typecase x
      (real (fail))
      (variable (restrict-nonreal! x))
      (otherwise))))

(defun assert!-numberpv (x)
  (let ((x (value-of x)))
    (typecase x
      (number)
      (variable (restrict-number! x))
      (otherwise (fail)))))

(defun assert!-notv-numberpv (x)
  (let ((x (value-of x)))
    (typecase x
      (number (fail))
      (variable (restrict-nonnumber! x))
      (otherwise))))

(defun assert!-booleanpv (x)
  (let ((x (value-of x)))
    (typecase x
      (boolean)
      (variable (restrict-boolean! x))
      (otherwise (fail)))))

(defun assert!-notv-booleanpv (x)
  (let ((x (value-of x)))
    (typecase x
      (boolean (fail))
      (variable (restrict-nonboolean! x))
      (otherwise))))

;;; Lifted Arithmetic Comparison Functions (Two argument ASSERT! optimized)

(defun assert!-=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (let ((x (variablize x))
        (y (variablize y)))
    (attach-noticer! #'(lambda () (=-rule x y)) x)
    (attach-noticer! #'(lambda () (=-rule x y)) y)))

(defun assert!-<=v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (let ((x (variablize x))
        (y (variablize y)))
    (attach-noticer! #'(lambda () (<=-rule x y)) x)
    (attach-noticer! #'(lambda () (<=-rule x y)) y)))

(defun assert!-<v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (let ((x (variablize x))
        (y (variablize y)))
    (attach-noticer! #'(lambda () (<-rule x y)) x)
    (attach-noticer! #'(lambda () (<-rule x y)) y)))

(defun assert!-/=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (let ((x (variablize x))
        (y (variablize y)))
    ;; NOTE: Got rid of the nondeterministic version that called the
    ;;       nondeterministic version of /=-RULE.
    (attach-noticer! #'(lambda () (/=-rule x y)) x)
    (attach-noticer! #'(lambda () (/=-rule x y)) y)))

;;; Lifted Type Functions

(defun integerpv (x)
  "Returns T if X is known to be integer valued, and NIL if X is known be
non-integer value.

If it is not known whether or not X is integer valued when INTEGERPV is called
then INTEGERPV creates and returns a new boolean variable V.

The values of X and V are mutually constrained via noticers so that V is equal
to T if and only if X is known to be integer valued, and V is equal to NIL if
and only if X is known to be non-integer valued.

If X later becomes known to be integer valued, a noticer attached to X
restricts V to equal T. Likewise, if X later becomes known to be non-integer
valued, a noticer attached to X restricts V to equal NIL.

Furthermore, if V ever becomes known to equal T then a noticer attached to V
restricts X to be integer valued. Likewise, if V ever becomes known to equal
NIL then a noticer attached to V restricts X to be non-integer valued."
  (cond ((known?-integerpv x) t)
        ((known?-notv-integerpv x) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-integer? x) (restrict-true! z))
                        ((variable-noninteger? x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (restrict-integer! x))
                        ((variable-false? z) (restrict-noninteger! x))))
              z)
             z))))

(defun realpv (x)
  "Returns T if X is known to be real, NIL if X is known to be non-real,
and otherwise returns a new boolean variable V.

The values of X and V are mutually constrained via noticers so that V is equal
to T if and only if X is known to be real and V is equal to NIL if and only if
X is known to be non-real.

* If X later becomes known to be real, a noticer attached to X restricts V to
  equal T. Likewise, if X later becomes known to be non-real, a noticer
  attached to X restricts V to equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X
  to be real. Likewise, if V ever becomes known to equal NIL then a noticer
  attached to V restricts X to be non-real."
  (cond ((known?-realpv x) t)
        ((known?-notv-realpv x) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-real? x) (restrict-true! z))
                        ((variable-nonreal? x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (restrict-real! x))
                        ((variable-false? z) (restrict-nonreal! x))))
              z)
             z))))

(defun numberpv (x)
  "Returns T if X is known to be numeric, NIL if X is known to be
non-numeric, and otherwise returns a new boolean variable V.

The values of X and V are mutually constrained via noticers so that V is equal
to T if and only if X is known to be numeric and V is equal to NIL if and only
if X is known to be non-numeric.

* If X later becomes known to be numeric, a noticer attached to X restricts V
  to equal T. Likewise, if X later becomes known to be non-numeric, a noticer
  attached to X restricts V to equal NIL.

* If V ever becomes known
  to equal T then a noticer attached to V restricts X to be numeric. Likewise,
  if V ever becomes known to equal NIL then a noticer attached to V restricts X
  to be non-numeric."
  (cond ((known?-numberpv x) t)
        ((known?-notv-numberpv x) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-number? x) (restrict-true! z))
                        ((variable-nonnumber? x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (restrict-number! x))
                        ((variable-false? z) (restrict-nonnumber! x))))
              z)
             z))))

(defun booleanpv (x)
  "The expression \(BOOLEANPV X) is an abbreviation for \(MEMBERV X '\(T NIL))."
  (cond ((known?-booleanpv x) t)
        ((known?-notv-booleanpv x) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-boolean? x) (restrict-true! z))
                        ((variable-nonboolean? x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (restrict-boolean! x))
                        ((variable-false? z) (restrict-nonboolean! x))))
              z)
             z))))

;;; Lifted MEMBERV

(defun known?-memberv-list-internal (x y)
  (and (consp y)
       (or (known?-equalv x (first y))
           (known?-memberv-list-internal x (rest y)))))

(defun known?-memberv-list (x y)
  (typecase y
    (cons (or (known?-equalv x (first y)) (known?-memberv-list x (rest y))))
    (variable
     (if (eq (variable-value y) y)
         (and (not (eq (variable-enumerated-domain y) t))
              (every
               #'(lambda (element) (known?-memberv-list-internal x element))
               (variable-enumerated-domain y)))
         (known?-memberv-list x (variable-value y))))
    (otherwise nil)))

(defun known?-memberv-internal (x y)
  (typecase y
    (list (known?-memberv-list x y))
    (vector (some #'(lambda (element) (known?-equalv x element)) y))
    (variable
     (if (eq (variable-value y) y)
         (and (not (eq (variable-enumerated-domain y) t))
              (every
               #'(lambda (element)
                   (typecase element
                     (list (known?-memberv-list-internal x element))
                     (vector (some #'(lambda (e) (known?-equalv x e)) element))
                     (otherwise nil)))
               (variable-enumerated-domain y)))
         (known?-memberv-internal x (variable-value y))))
    (otherwise (fail))))

(defun known?-memberv (x y)
  (cond ((and (variable? x) (not (eq (variable-value x) x)))
         (known?-memberv (variable-value x) y))
        ((and (variable? x) (not (eq (variable-enumerated-domain x) t)))
         ;; NOTE: This first alternative is an optimization in case membership
         ;;       can be determined simply through sharing relationships.
         (or (known?-memberv-internal x y)
             (every #'(lambda (element) (known?-memberv-internal element y))
                    (variable-enumerated-domain x))))
        (t (known?-memberv-internal x y))))

(defun known?-notv-memberv-list-internal (x y)
  (or (not (consp y))
      (and (known?-notv-equalv x (first y))
           (known?-notv-memberv-list-internal x (rest y)))))

(defun known?-notv-memberv-list (x y)
  (typecase y
    (cons (and (known?-notv-equalv x (first y))
               (known?-notv-memberv-list x (rest y))))
    (variable
     (if (eq (variable-value y) y)
         (and (not (eq (variable-enumerated-domain y) t))
              (every #'(lambda (element)
                         (known?-notv-memberv-list-internal x element))
                     (variable-enumerated-domain y)))
         (known?-notv-memberv-list x (variable-value y))))
    (otherwise t)))

(defun known?-notv-memberv-internal (x y)
  (typecase y
    (list (known?-notv-memberv-list x y))
    (vector (every #'(lambda (element) (known?-notv-equalv x element)) y))
    (variable
     (if (eq (variable-value y) y)
         (and (not (eq (variable-enumerated-domain y) t))
              (every
               #'(lambda (element)
                   (typecase element
                     (list (known?-notv-memberv-list-internal x element))
                     (vector
                      (every #'(lambda (e) (known?-notv-equalv x e)) element))
                     (otherwise nil)))
               (variable-enumerated-domain y)))
         (known?-notv-memberv-internal x (variable-value y))))
    (otherwise (fail))))

(defun known?-notv-memberv (x y)
  (cond
    ((and (variable? x) (not (eq (variable-value x) x)))
     (known?-notv-memberv (variable-value x) y))
    ((and (variable? x) (not (eq (variable-enumerated-domain x) t)))
     ;; NOTE: This first alternative is an optimization in case membership
     ;;       can be determined simply through sharing relationships.
     (or (known?-notv-memberv-internal x y)
         (every #'(lambda (element) (known?-notv-memberv-internal element y))
                (variable-enumerated-domain x))))
    (t (known?-notv-memberv-internal x y))))

(defun assert!-memberv-internal (x y)
  (let ((x (value-of x)))
    (if (known?-notv-memberv x y) (fail))
    (if (variable? x)
        (let ((y (value-of y)))
          (unless (variable? y) (restrict-enumerated-domain! x y))))))

(defun assert!-memberv (x y)
  (let ((y (value-of y)))
    (if (vectorp y)
        (dotimes (i (length y))
          (attach-noticer! #'(lambda () (assert!-memberv-internal x y))
                           (aref y i)))
        (attach-noticer! #'(lambda () (assert!-memberv-internal x y)) y))))

(defun assert!-notv-memberv-internal (x y)
  (let ((x (value-of x)))
    (if (known?-memberv x y) (fail))
    (if (variable? x)
        (let ((y (value-of y)))
          (unless (variable? y) (restrict-enumerated-antidomain! x y))))))

(defun assert!-notv-memberv (x y)
  (let ((y (value-of y)))
    (if (vectorp y)
        (dotimes (i (length y))
          (attach-noticer! #'(lambda () (assert!-notv-memberv-internal x y))
                           (aref y i)))
        (attach-noticer! #'(lambda () (assert!-notv-memberv-internal x y)) y))))

(defun memberv (x sequence)
  "Returns T if X is known to be a member of SEQUENCE \(using the Common Lisp
function EQL as a test function), NIL if X is known not to be a member of
SEQUENCE, and otherwise returns a new boolean variable V.

When a new variable is created, the values of X and V are mutually constrained
via noticers so that V is equal to T if and only if X is known to be a member
of SEQUENCE and V is equal to NIL if and only if X is known not to be a member
of SEQUENCE.

* If X later becomes known to be a member of SEQUENCE, a noticer attached to X
  restricts v to equal T. Likewise, if X later becomes known not to be a
  member of SEQUENCE, a noticer attached to X restricts V to equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X
  to be a member of SEQUENCE. Likewise, if V ever becomes known to equal NIL
  then a noticer attached to V restricts X not to be a member of SEQUENCE.

The current implementation imposes two constraints on the parameter SEQUENCE.
First, SEQUENCE must be bound when MEMBERV is called. Second, SEQUENCE must
not contain any unbound variables when MEMBERV is called.

The value of parameter SEQUENCE must be a sequence, i.e. either a list or a
vector."
  (cond ((known?-memberv x sequence) t)
        ((known?-notv-memberv x sequence) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-memberv x sequence) (restrict-true! z))
                        ((known?-notv-memberv x sequence) (restrict-false! z))))
              x)
             (if (vectorp sequence)
                 (map nil (lambda (element)
                            (attach-noticer!
                             #'(lambda ()
                                 (cond ((known?-memberv x sequence) (restrict-true! z))
                                       ((known?-notv-memberv x sequence) (restrict-false! z))))
                             element)) sequence)
                 (attach-noticer!
                  #'(lambda ()
                      (cond ((known?-memberv x sequence) (restrict-true! z))
                            ((known?-notv-memberv x sequence) (restrict-false! z))))
                  sequence))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-memberv x sequence))
                        ((variable-false? z) (assert!-notv-memberv x sequence))))
              z
              :dependencies (list x))
             z))))

;;; Lifted Arithmetic Comparison Functions (Two argument optimized)

(defun =v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (cond ((known?-=v2-internal x y) t)
        ((known?-/=v2-internal x y) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-=v2-variable x y) (restrict-true! z))
                        ((known?-/=v2-variable x y) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-=v2-variable x y) (restrict-true! z))
                        ((known?-/=v2-variable x y) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-=v2 x y))
                        ((variable-false? z) (assert!-/=v2 x y))))
              z
              :dependencies (list x y))
             z))))

(defun <=v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (cond ((known?-<=v2-internal x y) t)
        ((known?-<v2-internal y x) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-<=v2-variable x y) (restrict-true! z))
                        ((known?-<v2-variable y x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-<=v2-variable x y) (restrict-true! z))
                        ((known?-<v2-variable y x) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-<=v2 x y))
                        ((variable-false? z) (assert!-<v2 y x))))
              z
              :dependencies (list x y))
             z))))

(defun <v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (cond ((known?-<v2-internal x y) t)
        ((known?-<=v2-internal y x) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-<v2-variable x y) (restrict-true! z))
                        ((known?-<=v2-variable y x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-<v2-variable x y) (restrict-true! z))
                        ((known?-<=v2-variable y x) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-<v2 x y))
                        ((variable-false? z) (assert!-<=v2 y x))))
              z
              :dependencies (list x y))
             z))))

(defun /=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (cond ((known?-/=v2-internal x y) t)
        ((known?-=v2-internal x y) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-/=v2-variable x y) (restrict-true! z))
                        ((known?-=v2-variable x y) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-/=v2-variable x y) (restrict-true! z))
                        ((known?-=v2-variable x y) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-/=v2 x y))
                        ((variable-false? z) (assert!-=v2 x y))))
              z
              :dependencies (list x y))
             z))))

;;; Lifted NOTV, ANDV and ORV

(defun notv (x)
  "Restricts X to be a boolean.

Returns T if this restricts X to NIL, and T if this restricts X to NIL.

Otherwise returns a new boolean variable V. V and X are mutually constrained
via noticers, so that if either is later known to equal T, the other is
restricted to equal NIL and vice versa.

Note that unlike CL:NOT NOTV does not accept arbitrary values as arguments: it
fails if its argument is not T, NIL, or variable that can be restricted to a
boolean."
  (assert!-booleanpv x)
  (let ((x (value-of x)))
    (cond ((eq x t) nil)
          ((eq x nil) t)
          (t (let ((z (a-booleanv)))
               (attach-noticer!
                #'(lambda ()
                    (cond ((variable-true? x) (restrict-false! z))
                          ((variable-false? x) (restrict-true! z))))
                x)
               (attach-noticer!
                #'(lambda ()
                    (cond ((variable-true? z) (restrict-false! x))
                          ((variable-false? z) (restrict-true! x))))
                z
                :dependencies (list x))
               z)))))

(defun andv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (if (member nil xs :test #'eq)
        nil
        (let* ((xs (remove t xs :test #'eq))
               (count (length xs)))
          (cond
            ((zerop count) t)
            ((= count 1) (first xs))
            (t (let ((z (a-booleanv)))
                 (attach-noticer!
                  #'(lambda ()
                      (cond ((variable-true? z) (dolist (x xs) (restrict-true! x)))
                            ((and (= count 1) (variable-false? z))
                             (dolist (x xs)
                               (unless (variable-true? x) (restrict-false! x))))))
                  z
                  :dependencies xs)
                 (dolist (x xs)
                   (let ((x x))
                     (attach-noticer!
                      #'(lambda ()
                          (cond ((variable-false? x) (restrict-false! z))
                                ((variable-true? x)
                                 (local (decf count))
                                 (cond ((zerop count) (restrict-true! z))
                                       ((and (= count 1) (variable-false? z))
                                        (dolist (x xs)
                                          (unless (variable-true? x)
                                            (restrict-false! x))))))))
                      x)))
                 z)))))))

(defun andv (&rest xs)
  "Restricts each argument to be boolean.

Returns T if called with no arguments, or if all arguments are known to equal
T after being restricted to be boolean, and returns NIL if any argument is
known to equal NIL after this restriction.

Otherwise returns a boolean variable V. The values of the arguments and V are
mutually constrained:

 * If any argument is later known to equal NIL value of V becomes NIL.

 * If all arguments are later known to equal T, value of V becomes T.

 * If value of V is later known to equal T, all arguments become T.

 * If value of V is later known to equal NIL, and all but one argument is
   known to be T, the remaining argument becomes NIL.

Note that unlike CL:AND, ANDV is a function and always evaluates all its
arguments. Secondly, any non-boolean argument causes it to fail."
  (andv-internal xs))

(defun assert!-notv-andv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (unless (member nil xs :test #'eq)
      (let* ((xs (remove t xs :test #'eq))
             (count (length xs)))
        (cond ((zerop count) (fail))
              ((= count 1) (restrict-false! (first xs)))
              (t (dolist (x xs)
                   (let ((x x))
                     (attach-noticer!-internal
                      #'(lambda ()
                          (cond ((variable-false? x))
                                ((variable-true? x)
                                 (local (decf count))
                                 (cond ((zerop count) (fail))
                                       ((= count 1)
                                        (dolist (x xs)
                                          (unless (variable-true? x)
                                            (restrict-false! x))))))))
                      x)))))))))

(defun assert!-notv-andv (&rest xs) (assert!-notv-andv-internal xs))

(defun orv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (if (member t xs :test #'eq)
        t
        (let* ((xs (remove nil xs :test #'eq))
               (count (length xs)))
          (cond
            ((zerop count) nil)
            ((= count 1) (first xs))
            (t (let ((z (a-booleanv)))
                 (attach-noticer!
                  #'(lambda ()
                      (cond ((variable-false? z)
                             (dolist (x xs) (restrict-false! x)))
                            ((and (= count 1) (variable-true? z))
                             (dolist (x xs)
                               (unless (variable-false? x) (restrict-true! x))))))
                  z)
                 (dolist (x xs)
                   (let ((x x))
                     (attach-noticer!
                      #'(lambda ()
                          (cond ((variable-true? x) (restrict-true! z))
                                ((variable-false? x)
                                 (local (decf count))
                                 (cond ((zerop count) (restrict-false! z))
                                       ((and (= count 1) (variable-true? z))
                                        (dolist (x xs)
                                          (unless (variable-false? x)
                                            (restrict-true! x))))))))
                      x)))
                 z)))))))

(defun orv (&rest xs)
  "Restricts each argument to be boolean.

Returns NIL if called with no arguments, or if all arguments are known to
equal NIL after being restructed to be boolean, and returns T if any argument
is known to equal T after this restriction.

Otherwise returns a boolean variable V. The values of arguments and V are
mutually constrained:

 * If any argument is later known to equal T, value of V becomes T.

 * If all arguments are later known to equal NIL, value of V becomes NIL.

 * If value of V is later known to equal NIL, all arguments become NIL.

 * If value of V is later known to equal T, and all but one argument is
   known to be NIL, the remaining argument becomes T.

Note that unlike CL:OR, ORV is a function and always evaluates all its
arguments. Secondly, any non-boolean argument causes it to fail."
  (orv-internal xs))

(defun assert!-orv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (unless (member t xs :test #'eq)
      (let* ((xs (remove nil xs :test #'eq))
             (count (length xs)))
        (cond ((zerop count) (fail))
              ((= count 1) (restrict-true! (first xs)))
              (t (dolist (x xs)
                   (let ((x x))
                     (attach-noticer!-internal
                      #'(lambda ()
                          (cond ((variable-true? x))
                                ((variable-false? x)
                                 (local (decf count))
                                 (cond ((zerop count) (fail))
                                       ((= count 1)
                                        (dolist (x xs)
                                          (unless (variable-false? x)
                                            (restrict-true! x))))))))
                      x)))))))))

(defun assert!-orv (&rest xs) (assert!-orv-internal xs))

(defun assert!-clause (xs ps)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (unless (some #'eq xs ps)
      (let (new-xs new-ps)
        (do ((xrest xs (rest xrest))
             (prest ps (rest prest)))
            ((or (null xrest) (null prest)))
          (let ((x (first xrest))
                (p (first prest)))
            (unless (eq x (not p))
              (push x new-xs)
              (push p new-ps))))
        (let ((count (length new-xs)))
          (cond ((zerop count) (fail))
                ((= count 1)
                 (if (first new-ps)
                     (restrict-true! (first new-xs))
                     (restrict-false! (first new-xs))))
                (t (do ((xrest new-xs (rest xrest))
                        (prest new-ps (rest prest)))
                       ((null xrest))
                     (let ((x (first xrest)))
                       (attach-noticer!-internal
                        (if (first prest)
                            #'(lambda ()
                                (cond ((variable-true? x))
                                      ((variable-false? x)
                                       (local (decf count))
                                       (cond ((zerop count) (fail))
                                             ((= count 1)
                                              (do ((xrest new-xs (rest xrest))
                                                   (prest new-ps (rest prest)))
                                                  ((null xrest))
                                                (let ((x (first xrest)))
                                                  (unless (bound? x)
                                                    (if (first prest)
                                                        (restrict-true! x)
                                                        (restrict-false! x))))))))))
                            #'(lambda ()
                                (cond ((variable-false? x))
                                      ((variable-true? x)
                                       (local (decf count))
                                       (cond
                                         ((zerop count) (fail))
                                         ((= count 1)
                                          (do ((xrest new-xs (rest xrest))
                                               (prest new-ps (rest prest)))
                                              ((null xrest))
                                            (let ((x (first xrest)))
                                              (unless (bound? x)
                                                (if (first prest)
                                                    (restrict-true! x)
                                                    (restrict-false! x)))))))))))
                        x))))))))))

(defun count-trues-internal (xs)
  (count-if #'identity xs))

(defun count-trues (&rest xs)
  "Returns the number of time a non-NIL value occurs in its arguments."
  (count-trues-internal xs))

(defun count-truesv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs))
        (lower 0)
        (upper (length xs)))
    (dolist (x xs)
      (cond ((eq x t) (incf lower))
            ((eq x nil) (decf upper))))
    (if (= lower upper)
        lower
        (let ((z (an-integer-betweenv lower upper))
              (xs (remove-if #'bound? xs)))
          (attach-noticer!
           #'(lambda ()
               (if (= upper (variable-lower-bound z))
                   (dolist (x xs)
                     (unless (variable-false? x) (restrict-true! x))))
               (if (= lower (variable-upper-bound z))
                   (dolist (x xs)
                     (unless (variable-true? x) (restrict-false! x)))))
           z
           :dependencies xs)
          (dolist (x xs)
            (let ((x x))
              (attach-noticer!
               #'(lambda ()
                   (cond ((variable-false? x)
                          (local (decf upper))
                          (restrict-upper-bound! z upper))
                         ((variable-true? x)
                          (local (incf lower))
                          (restrict-lower-bound! z lower))))
               x)))
          z))))

(defun count-truesv (&rest xs)
  "Constrains all its arguments to be boolean. If each argument is known, returns
the number of T arguments. Otherwise returns a fresh constraint variable V.

V and arguments are mutually constrained:

 * Lower bound of V is the number arguments known to be T.

 * Upper bound of V is the number arguments minus the number of arguments known to be NIL.

 * If lower bound of V is constrained to be equal to number of arguments known
   to be NIL, all arguments not known to be NIL are constrained to be T.

 * If Upper bound of V is constrained to be equal to number of arguments known
   to be T, all arguments not known to be T are constrained to be NIL."
  (count-truesv-internal xs))

;;; Lifted FUNCALLV and APPLYV

(defun finite-domain? (variable)
  (let ((variable (value-of variable)))
    (or (not (variable? variable))
        (not (eq (variable-enumerated-domain variable) t))
        (and (variable-integer? variable)
             (variable-lower-bound variable)
             (variable-upper-bound variable)))))

;;; NOTE: SOLUTION, LINEAR-FORCE and STATIC-ORDERING were moved here to be
;;;       before KNOWN?-CONSTRAINT to avoid forward references to
;;;       nondeterministic functions.

(defun solution (arguments ordering-force-function)
  "ARGUMENTS is a list of values. Typically it is a list of
variables but it may also contain nonvariables.

The specified ORDERING-FORCE-FUNCTION is used to force each of the variables
in list to be bound.

Returns a list of the values of the elements of list in the same order that
they appear in list, irrespective of the forcing order imposed by the
ORDERING-FORCE-FUNCTION.

The ORDERING-FORCE-FUNCTION can be any function which takes a list of values
as its single argument that is guaranteed to force all variables in that list
to be bound upon its return. The returned value of the ORDERING-FORCE-FUNCTION
is ignored.

The user can construct her own ORDERING-FORCE-FUNCTION or use one of the
following alternatives provided with Screamer:

   \(STATIC-ORDERING #'LINEAR-FORCE),
   \(STATIC-ORDERING #'DIVIDE-AND-CONQUER-FORCE),
   \(REORDER COST-FUN TERMINATE-TEST ORDER #'LINEAR-FORCE) and
   \(REORDER COST-FUN TERMINATE-TEST ORDER #'DIVIDE-AND-CONQUER-FORCE).

Future implementation of Screamer may provide additional forcing and ordering
functions."
  (funcall-nondeterministic
   (value-of ordering-force-function)
   (variables-in (value-of arguments)))
  (apply-substitution arguments))

(defun linear-force (x)
  "Returns X if it is not a variable. If X is a bound variable then returns
its value.

If X is an unbound variable then it must be known to have a countable set of
potential values. In this case X is nondeterministically restricted to be
equal to one of the values in this countable set, thus forcing X to be bound.
The dereferenced value of X is then returned.

An unbound variable is known to have a countable set of potential values
either if it is known to have a finite domain or if it is known to be integer
valued.

An error is signalled if X is not known to have a finite domain and is not
known to be integer valued.

Upon backtracking X will be bound to each potential value in turn, failing
when there remain no untried alternatives.

Since the set of potential values is required only to be countable, not
finite, the set of untried alternatives may never be exhausted and
backtracking need not terminate. This can happen, for instance, when X is
known to be an integer but lacks either an upper of lower bound.

The order in which the nondeterministic alternatives are tried is left
unspecified to give future implementations leeway in incorporating heuristics
in the process of determining a good search order."
  (let ((variable (value-of x)))
    (when (variable? variable)
      (restrict-value!
       variable
       (cond ((not (eq (variable-enumerated-domain variable) t))
              (a-member-of (variable-enumerated-domain variable)))
             ((variable-integer? variable)
              (if (variable-lower-bound variable)
                  (if (variable-upper-bound variable)
                      (an-integer-between
                       (variable-lower-bound variable)
                       (variable-upper-bound variable))
                      (an-integer-above (variable-lower-bound variable)))
                  (if (variable-upper-bound variable)
                      (an-integer-below (variable-upper-bound variable))
                      (an-integer))))
             (t (error "It is only possible to linear force a variable that~%~
                        has a countable domain"))))))
  (value-of variable))

(defun static-ordering-internal (variables force-function)
  (when variables
    (let ((variable (value-of (first variables))))
      (etypecase variable
        (variable
         (funcall-nondeterministic force-function variable)
         (static-ordering-internal variables force-function))
        (t (static-ordering-internal (rest variables) force-function))))))

(defun static-ordering (force-function)
  "Returns an ordering force function based on FORCE-FUNCTION.

The ordering force function which is returned is a nondeterministic function
which takes a single argument X. This argument X can be a list of values where
each value may be either a variable or a non-variable. The ordering force
function applies the FORCE-FUNCTION in turn to each of the variables in X, in
the order that they appear, repeatedly applying the FORCE-FUNCTION to a given
variable until it becomes bound before proceeding to the next variable. The
ordering force function does not return any meaningful result.

FORCE-FUNCTION is any (potentially nondeterministic) function which can be
applied to a variable as its single argument with the stipulation that a
finite number of repeated applications will force the variable to be bound.
The FORCE-FUNCTION need not return any useful value.

Screamer currently provides two convenient force-functions, namely
#'LINEAR-FORCE and #'DIVIDE-AND-CONQUER-FORCE though future implementations
may provide additional ones. \(The defined Screamer protocol does not provide
sufficient hooks for the user to define her own force functions.)"
  ;; NOTE: This closure will heap cons.
  (let ((force-function (value-of force-function)))
    #'(lambda (variables)
        ;; Force the dependencies and then the target variables
        (static-ordering-internal (get-variable-dependency-closure variables) force-function))))

(defun known?-constraint (f polarity? x)
  (let ((f (value-of f)))
    (if (variable? f)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV or APPLYV to be an unbound variable"))
    (unless (functionp f)
      (error "The first argument to FUNCALLV or APPLYV must be a deterministic~%~
           function"))
    (and (every #'finite-domain? x)
         (block exit
           (for-effects
             (if (if polarity?
                     (not (apply f (solution x (static-ordering #'linear-force))))
                     (apply f (solution x (static-ordering #'linear-force))))
                 (return-from exit nil)))
           t))))

(defun propagate-gfc (predicate polarity? variables unassigned-variable)
  ;; NOTE: UNASSIGNED-VARIABLE must be a variable which is not bound and
  ;;       all of the VARIABLES except the UNASSIGNED-VARIABLE must be bound.
  (let ((unassigned-variable (value-of unassigned-variable)))
    ;; There is no way to propagate a value to a variable that doesn't have an
    ;; enumerated domain.
    (if (and (not (eq (variable-enumerated-domain unassigned-variable) t))
             (not (null (rest (variable-enumerated-domain
                               unassigned-variable)))))
        ;; NOTE: Could do less consing if had LOCAL DELETE-IF-NOT.
        ;; NOTE: Consing.
        (let* ((variable-values (mapcar #'value-of variables))
               (new-enumerated-domain
                 (if polarity?
                     (remove-if-not
                      #'(lambda (value)
                          (apply predicate
                                 ;; NOTE: Consing.
                                 (mapcar #'(lambda (variable variable-value)
                                             (if (eq variable unassigned-variable)
                                                 value
                                                 variable-value))
                                         variables
                                         variable-values)))
                      (variable-enumerated-domain unassigned-variable))
                     (remove-if
                      #'(lambda (value)
                          (apply predicate
                                 ;; NOTE: Consing.
                                 (mapcar #'(lambda (variable variable-value)
                                             (if (eq variable unassigned-variable)
                                                 value
                                                 variable-value))
                                         variables
                                         variable-values)))
                      (variable-enumerated-domain unassigned-variable)))))
          (list variable-values)
          (if (set-enumerated-domain! unassigned-variable new-enumerated-domain)
              (run-noticers unassigned-variable))))))

(defun a-tuple (variables variable value)
  (unless (null variables)
    (cons (cond ((eq (first variables) variable) value)
                       ((variable? (first variables))
                        (a-member-of (variable-enumerated-domain (first variables))))
                       (t (first variables)))
                 (a-tuple (rest variables) variable value))))

(defun propagate-ac (predicate polarity? variables)
  (unless (some #'(lambda (variable)
                    (and (variable? variable)
                         (eq (variable-enumerated-domain variable) t)))
                variables)
    (dolist (variable variables)
      ;; NOTE: Could do less consing if had LOCAL DELETE-IF-NOT.
      (when (variable? variable)
        (let* ((pred-func #'(lambda (value)
                              (possibly?
                                ;; NOTE: Consing.
                                (apply predicate (a-tuple variables variable value)))))
               (new-enumerated-domain
                 (if polarity?
                     (remove-if-not pred-func (variable-enumerated-domain variable))
                     (remove-if pred-func (variable-enumerated-domain variable)))))
          (if (set-enumerated-domain! variable new-enumerated-domain)
              (run-noticers variable)))))))

(defun assert!-constraint-gfc (predicate polarity? variables)
  (let ((predicate (value-of predicate))
        (multiple-unassigned-variables? nil)
        (unassigned-variable nil))
    (if (variable? predicate)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV or APPLYV to be an unbound variable"))
    (unless (functionp predicate)
      (error "The first argument to FUNCALLV or APPLYV must be a deterministic~%~
           function"))
    (dolist (variable variables)
      (unless (bound? variable)
        (when unassigned-variable (setf multiple-unassigned-variables? t))
        (setf unassigned-variable variable)))
    (cond
      (multiple-unassigned-variables?
       ;; The case where two or more variables are unbound
       (let ((variables (copy-list variables)))
         (dolist (variable variables)
           (unless (bound? variable)
             (let ((variable variable))
               (attach-noticer!
                #'(lambda ()
                    (global
                      (block exit
                        (let ((unassigned-variable nil))
                          (dolist (variable variables)
                            (unless (bound? variable)
                              (if unassigned-variable (return-from exit))
                              (setf unassigned-variable variable)))
                          (if unassigned-variable
                              (propagate-gfc
                               predicate polarity? variables unassigned-variable)
                              (unless (if polarity?
                                          (apply predicate (mapcar #'value-of variables))
                                          (not (apply predicate
                                                      (mapcar #'value-of variables))))
                                (fail)))))))
                variable))))))
      (unassigned-variable
       ;; The case where all but one of the variables are bound
       (propagate-gfc predicate polarity? variables unassigned-variable))
      ;; The case where all variables are bound
      ;; NOTE: Consing.
      (t (unless (if polarity?
                     (apply predicate (mapcar #'value-of variables))
                     (not (apply predicate (mapcar #'value-of variables))))
           (fail))))))

(defun assert!-constraint-ac (predicate polarity? variables)
  (let ((predicate (value-of predicate)))
    (if (variable? predicate)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV or APPLYV to be an unbound variable"))
    (unless (functionp predicate)
      (error "The first argument to FUNCALLV or APPLYV must be a deterministic~%~
           function"))
    (dolist (variable variables)
      (attach-noticer!
       #'(lambda () (propagate-ac predicate polarity? variables))
       variable))
    (propagate-ac predicate polarity? variables)))

(defun assert!-constraint (predicate polarity? variables)
  (ecase *strategy*
    (:gfc (assert!-constraint-gfc predicate polarity? variables))
    (:ac (assert!-constraint-ac predicate polarity? variables))))

(defun known?-funcallv (f &rest x) (known?-constraint f t x))

(defun known?-notv-funcallv (f &rest x) (known?-constraint f nil x))

(defun assert!-funcallv (f &rest x) (assert!-constraint f t x))

(defun assert!-notv-funcallv (f &rest x) (assert!-constraint f nil x))

(defun funcallv (f &rest x)
  "F must be a deterministic function. If all arguments X are bound, returns
the result of calling F on the dereferenced values of arguments.

Otherwise returns a fresh variable V, constrained to be equal to the result
of calling F on the dereferenced values of arguments.

Additionally, if all but one of V and the argument variables become known, and
the remaining variable has a finite domain, then that domain is further
restricted to be consistent with other arguments."
  (let ((f (value-of f)))
    (if (variable? f)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV to be an unbound variable"))
    (unless (functionp f)
      (error "The first argument to FUNCALLV must be a deterministic function"))
    (if (every #'bound? x)
        (apply f (mapcar #'value-of x))
        (let ((z (make-variable)))
          (assert!-constraint
           #'(lambda (&rest x) (equal (first x) (apply f (rest x)))) t (cons z x))
          (dolist (argument x)
            (attach-noticer!
             #'(lambda ()
                 (if (every #'bound? x)
                     (assert!-equalv z (apply f (mapcar #'value-of x)))))
             argument))
          z))))

(defun arguments-for-applyv (x xs)
  (unless (bound? (first (last (cons x xs))))
    (error "The current implementation does not allow the last argument to~%~
          APPLYV to be an unbound variable"))
  (apply #'list* (mapcar #'value-of (cons x xs))))

(defun known?-applyv (f x &rest xs)
  (known?-constraint f t (arguments-for-applyv x xs)))

(defun known?-notv-applyv (f x &rest xs)
  (known?-constraint f nil (arguments-for-applyv x xs)))

(defun assert!-applyv (f x &rest xs)
  (assert!-constraint f t (arguments-for-applyv x xs)))

(defun assert!-notv-applyv (f x &rest xs)
  (assert!-constraint f nil (arguments-for-applyv x xs)))

(defun applyv (f x &rest xs)
  "F must be a deterministic function. If all arguments X are bound, returns
the result of calling F on the dereferenced values of spread arguments.

Otherwise returns a fresh variable V, constrained to be equal to the result
of calling F on the dereferenced values of arguments.

Additionally, if all but one of V and the argument variables become known, and
the remaining variable has a finite domain, then that domain is further
restricted to be consistent with other arguments."
  (let ((f (value-of f)))
    (if (variable? f)
        (error "The current implementation does not allow the first argument~%~
              of APPLYV to be an unbound variable"))
    (unless (functionp f)
      (error "The first argument to APPLYV must be a deterministic function"))
    (let ((arguments (arguments-for-applyv x xs)))
      (if (or
           ;; All arguments are bound
           (every #'bound? arguments)
           ;; TODO: Figure out how to mark functions as able to handle variable arguments
           ;; Hardcoded example condition:
           ;; (member f (list '+v #'+v))
           )
          (apply f (mapcar #'value-of arguments))
          (let ((z (make-variable)))
            (attach-noticer! nil z :dependencies (variables-in arguments))
            (assert!-constraint
             #'(lambda (&rest x) (equal (first x) (apply f (rest x))))
             t
             (cons z arguments))
            (dolist (argument arguments)
              (attach-noticer!
               #'(lambda ()
                   (when (every #'bound? arguments)
                     (assert!-equalv z (apply f (mapcar #'value-of arguments)))))
               argument))
            z)))))

;;; Lifted Arithmetic Functions

(defun +v-internal (xs)
  (cond ((null xs) 0)
        ((= 1 (length xs))
         (assert!-numberpv (first xs))
         (first xs))
        (t (+v2 (first xs) (+v-internal (rest xs))))))

(defun +v (&rest xs)
  "Constrains its arguments to be numbers. Returns 0 if called with no
arguments. If called with a single argument, returns its value. If called with
more than two arguments, behaves as nested sequence of two-argument calls:

  \(+V X1 X2 ... Xn) = \(+V X1 (+V X2 (+V ...)))

If any arguments are `eql' to each other, they are merged into a single
`*v' form  to reduce the chance of an infinite search in `solution'.

When called with two arguments, if both arguments are bound, returns the sum
of their values. If either argument is known to be zero, returns the value of
the remaining argument. Otherwise returns number variable V.

  * Sum of X1 and X2 is constrained to equal V. This includes constraining
    their bounds appropriately. If it becomes known that cannot be true, FAIL
    is called.

  * If both arguments are known to be reals, V is constrained to be real.

  * If both arguments are known to be integers, V is constained to be integer.

  * If one argument is known to be a non-integer, and the other is known to
    be a real, V is constrained to be a non-integer.

  * If one argument is known to be a non-real, and the other is known
    to be a real, V is constrained to be non-real.

Note: Numeric contagion rules of Common Lisp are not applied if either
argument equals zero."
  ;; FIXME: Changing this for testing the type system, need
  ;; to change it back afterwards.
  (+v-internal xs)
  ;; (let* ((uniques (sort (remove-duplicates xs)
  ;;                       ;; Put bound values first
  ;;                       (serapeum:op (and (bound? _) (not (bound? _))))))
  ;;        (counts (mapcar (rcurry #'count xs) uniques))
  ;;        (new-xs nil))
  ;;   (declare (dynamic-extent uniques counts))
  ;;   (dotimes (i (length uniques))
  ;;     (let ((c (nth i counts))
  ;;           (x (nth i uniques)))
  ;;       (push (*v x c) new-xs)))
  ;;   (+v-internal new-xs))
  )

(defun -v-internal (x xs)
  (if (null xs) x (-v-internal (-v2 x (first xs)) (rest xs))))

(defun -v (x &rest xs)
  "Constrains its arguments to be numbers. If called with a single argument,
behaves as if the two argument call:

  \(-V 0 X)

If called with more than two arguments, behaves as nested sequence of
two-argument calls:

  \(-V X1 X2 ... Xn) = \(-V X1 (-V X2 (-V ...)))

When called with two arguments, if both arguments are bound, returns the
difference of their values. If X2 is known to be zero, returns the value of
X1. Otherwise returns number variable V.

  * Difference of X1 and X2 is constrained to equal V. This includes
    constraining their bounds appropriately. If it becomes known that cannot
    be true, FAIL is called.

  * If both arguments are known to be reals, V is constrained to be real.

  * If both arguments are known to be integers, V is constained to be integer.

  * If one argument is known to be a non-integer, and the other is known to
    be a real, V is constrained to be a non-integer.

  * If one argument is known to be a non-real, and the other is known
    to be a real, V is constrained to be non-real.

Note: Numeric contagion rules of Common Lisp are not applied if X2 equals zero."
  (if (null xs) (-v2 0 x) (-v-internal x (list (apply #'+v xs)))))

(defun *v-internal (xs)
  (if (null xs) 1 (*v2 (first xs) (*v-internal (rest xs)))))

(defun *v (&rest xs)
  "Constrains its arguments to be numbers. If called with no arugments,
returns 1. If called with a single argument, returns its value. If called with
more than two arguments, behaves as nested sequence of two-argument calls:

  \(*V X1 X2 ... Xn) = \(*V X1 (*V X2 (*V ...)))

When called with two arguments, if both arguments are bound, returns the
product of their values. If either argument is known to equal zero, returns
zero. If either argument is known to equal one, returns the value of the other.
Otherwise returns number variable V.

  * Product of X1 and X2 is constrained to equal V. This includes constraining
    their bounds appropriately. If it becomes known that cannot be true, FAIL
    is called.

  * If both arguments are known to be reals, V is constrained to be real.

  * If both arguments are known to be integers, V is constained to be integer.

  * If V is known to be an integer, and either X1 or X2 is known to be real,
    both X1 and X2 are constrained to be integers.

  * If V is known to be an reals, and either X1 or X2 is known to be real,
    both X1 and X2 are constrained to be reals.

Note: Numeric contagion rules of Common Lisp are not applied if either
argument equals zero or one."
  (*v-internal xs))

(defun /v-internal (x xs)
  (if (null xs) x (/v-internal (/v2 x (first xs)) (rest xs))))

(defun /v (x &rest xs)
  "Constrains its arguments to be numbers. If called with a single argument,
behaves as the two argument call:

  \(/V 1 X)

If called with more than two arguments, behaves as nested sequence of
two-argument calls:

  \(/V X1 X2 ... Xn) = \(/V ... (/V (/V X1 X2) X3) ... Xn)

When called with two arguments, if both arguments are bound, returns the
division of their values. If X1 is known to equal zero, returns 0. If X2 is
known to equal zero, FAIL is called. If X2 is known to equal one, returns the
value of X1. Otherwise returns number variable V.

  * Division of X1 and X2 is constrained to equal V. This includes
    constraining their bounds appropriately. If it becomes known that cannot
    be true, FAIL is called.

  * If both arguments are known to be reals, V is constrained to be real.

  * If both arguments are known to be integers, V is constained to be integer.

  * If V is known to be an integer, and either X1 or X2 is known to be real,
    both X1 and X2 are constrained to be integers.

  * If V is known to be an reals, and either X1 or X2 is known to be real,
    both X1 and X2 are constrained to be reals.

Note: Numeric contagion rules of Common Lisp are not applied if X1 equals zero
or X2 equals one."
  (if (null xs) (/v2 1 x) (/v-internal x xs)))

(defun minv-internal (x xs)
  (cond ((null xs)
         (assert!-realpv x)
         (value-of x))
        (t
         (minv-internal (minv2 x (first xs)) (rest xs)))))

(defun minv (x &rest xs)
  "Constrains its arguments to be real. If called with a single argument,
returns its value. If called with multiple arguments, behaves as if a
combination of two argument calls:

  \(MINV X1 X2 ... Xn) == (MINV (MINV X1 X2) ... Xn)

If called with two arguments, and either is known to be less than or equal to
the other, returns the value of that argument. Otherwise returns a real variable
V, mutually constrained with the arguments:

  * Minimum of the values of X1 and X2 is constrained to equal V. This
    includes constraining their bounds appropriately. If it becomes know that
    cannot be true. FAIL is called.

  * If both arguments are integers, V is constrained to be an integer."
  (minv-internal x xs))

(defun maxv-internal (x xs)
  (cond ((null xs)
         (assert!-realpv x)
         (value-of x))
        (t
         (maxv-internal (maxv2 x (first xs)) (rest xs)))))

(defun maxv (x &rest xs)
  "Constrains its arguments to be real. If called with a single argument,
returns its value. If called with multiple arguments, behaves as if a
combination of two argument calls:

  \(MAXV X1 X2 ... Xn) == (MAXV (MAXV X1 X2) ... Xn)

If called with two arguments, and either is known to be greater than or equal
to the other, returns the value of that argument. Otherwise returns a real
variable V, mutually constrained with the arguments:

  * Maximum of the values of X1 and X2 is constrained to equal V. This
    includes constraining their bounds appropriately. If it becomes know that
    cannot be true. FAIL is called.

  * If both arguments are integers, V is constrained to be an integer."
  (maxv-internal x xs))

;;; Lifted Arithmetic Comparison Functions (KNOWN? optimized)

(defun known?-=v-internal (x xs)
  (if (null xs)
      t
      (and (known?-=v2 x (first xs))
           (known?-=v-internal (first xs) (rest xs)))))

(defun known?-=v (x &rest xs) (known?-=v-internal x xs))

(defun known?-<v-internal (x xs)
  (if (null xs)
      t
      (and (known?-<v2 x (first xs))
           (known?-<v-internal (first xs) (rest xs)))))

(defun known?-<v (x &rest xs) (known?-<v-internal x xs))

(defun known?-<=v-internal (x xs)
  (if (null xs)
      t
      (and (known?-<=v2 x (first xs))
           (known?-<=v-internal (first xs) (rest xs)))))

(defun known?-<=v (x &rest xs) (known?-<=v-internal x xs))

(defun known?->v-internal (x xs)
  (if (null xs)
      t
      (and (known?-<v2 (first xs) x)
           (known?->v-internal (first xs) (rest xs)))))

(defun known?->v (x &rest xs) (known?->v-internal x xs))

(defun known?->=v-internal (x xs)
  (if (null xs)
      t
      (and (known?-<=v2 (first xs) x)
           (known?->=v-internal (first xs) (rest xs)))))

(defun known?->=v (x &rest xs) (known?->=v-internal x xs))

(defun known?-/=v-internal (x xs)
  (if (null xs)
      t
      (and (known?-/=v2 x (first xs))
           (known?-/=v-internal x (rest xs))
           (known?-/=v-internal (first xs) (rest xs)))))

(defun known?-/=v (x &rest xs) (known?-/=v-internal x xs))

;;; Lifted Arithmetic Comparison Functions (ASSERT! optimized)

(defun assert!-=v-internal (x xs)
  (unless (null xs)
    (assert!-=v2 x (first xs))
    (assert!-=v-internal (first xs) (rest xs))))

(defun assert!-=v (x &rest xs) (assert!-=v-internal x xs))

(defun assert!-<v-internal (x xs)
  (unless (null xs)
    (assert!-<v2 x (first xs))
    (assert!-<v-internal (first xs) (rest xs))))

(defun assert!-<v (x &rest xs) (assert!-<v-internal x xs))

(defun assert!-<=v-internal (x xs)
  (unless (null xs)
    (assert!-<=v2 x (first xs))
    (assert!-<=v-internal (first xs) (rest xs))))

(defun assert!-<=v (x &rest xs) (assert!-<=v-internal x xs))

(defun assert!->v-internal (x xs)
  (unless (null xs)
    (assert!-<v2 (first xs) x)
    (assert!->v-internal (first xs) (rest xs))))

(defun assert!->v (x &rest xs) (assert!->v-internal x xs))

(defun assert!->=v-internal (x xs)
  (unless (null xs)
    (assert!-<=v2 (first xs) x)
    (assert!->=v-internal (first xs) (rest xs))))

(defun assert!->=v (x &rest xs) (assert!->=v-internal x xs))

(defun assert!-/=v-internal (x xs)
  (unless (null xs)
    (assert!-/=v2 x (first xs))
    (assert!-/=v-internal x (rest xs))
    (assert!-/=v-internal (first xs) (rest xs))))

(defun assert!-/=v (x &rest xs) (assert!-/=v-internal x xs))

;;; Lifted Arithmetic Comparisons Functions

(defun =v-internal (x xs)
  (if (null xs)
      t
      (andv (=v2 x (first xs)) (=v-internal (first xs) (rest xs)))))

(defun =v (x &rest xs)
  "Returns a boolean value which is constrained to be T if all of the
arguments are numerically equal, and constrained to be NIL if two or more of
the arguments numerically differ.

This function takes one or more arguments. All of the arguments are restricted
to be numeric.

Returns T when called with one argument. A call such as \(=V X1 X2 ... Xn)
with more than two arguments behaves like a conjunction of two argument calls:

  \(ANDV (=V X1 X2) ... (=V Xi Xi+1) ... (=V Xn-1 Xn))

When called with two arguments, returns T if X1 is known to be equal to X2 at
the time of call, NIL if X1 is known not to be equal to X2 at the time of
call, and a new boolean variable V if is not known if the two values are
equal.

Two numeric values are known to be equal only when they are both bound and
equal according to the Common Lisp function =.

Two numeric values are known not to be equal when their domains are disjoint.
Furthermore, two real values are known not to be equal when their ranges are
disjoint, i.e. the upper bound of one is greater than the lower bound of the
other.

When a new variable is created, the values of X1, X2, and V are mutually
constrained via noticers so that V is equal to T if and only if X1 is known to
be equal to X2, and V is equal to NIL if and only if X1 is known not to be
equal to X2.

* If it later becomes known that X1 is equal to X2 noticers attached to X1 and
  X2 restrict V to equal T. Likewise if it later becomes known that X1 is not
  equal to X2 noticers attached to X1 and X2 restrict V to equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X1
  to be equal to X2. Likewise if V ever becomes known to equal NIL then a
  noticer attached to V restricts X1 not to be equal to X2.

* If X1 is known to be real then the noticer attached to X2 continually
  restrict the upper bound of X1 to be no higher than the upper bound of X2
  and the lower bound of X1 to be no lower than the lower bound of X2.
  Likewise for bounds of X1 if X2 is known to be real.

Restricting two values x1 and x2 to be equal is performed by attaching
noticers to x1 and x2. These noticers continually restrict the domains of x1
and x2 to be equivalent sets (using the Common Lisp function = as a test
function) as their domains are restricted.

Restricting two values X1 and X2 to not be equal is also performed by
attaching noticers to X1 and X2. These noticers however do not restrict the
domains or ranges of X1 or X2. They simply monitor their continually
restrictions and fail when any assertion causes X1 to be known to be equal to
X2."
  (=v-internal x xs))

(defun <v-internal (x xs)
  (if (null xs)
      t
      (andv (<v2 x (first xs)) (<v-internal (first xs) (rest xs)))))

(defun <v (x &rest xs)
  "Returns a boolean value which is constrained to be T if each argument Xi is
less than the following argument Xi+1 and constrained to be NIL if some
argument Xi is greater than or equal to the following argument Xi+1.

This function takes one or more arguments. All of the arguments are restricted
to be real.

Returns T when called with one argument. A call such as \(<V X1 X2 ... Xn)
with more than two arguments behaves like a conjunction of two argument calls:

  \(ANDV \(<V X1 X2) ... \(<V Xi Xi+1 ) ... \(<V Xn-1 Xn))

When called with two arguments, returns T if X1 is known to be less than X2 at
the time of call, NIL if X1 is known to be greater than or equal to X2 at the
time of call, and otherwise a new boolean variable V.

A real value X1 is known to be less than a real value X2 if X1 has an upper
bound, X2 has a lower bound and the upper bound of X1 is less than the lower
bound of X2.

A real value X1 is known to be greater than or equal to a real value X2 if X1
has a lower bound, X2 has an upper bound and the lower bound of X1 is greater
than or equal to the upper bound of X2.

When a new variable is created, the values of X1, X2 and v are mutually
constrained via noticers so that V is equal to T if and only if X1 is known to
be less than X2 and V is equal to NIL if and only if X1 is known to be greater
than or equal to X2.

* If it later becomes known that X1 is less than X2, noticers attached to X1
  and X2 restrict V to equal T. Likewise, if it later becomes known that X1 is
  greater than or equal to X2, noticers attached to X1 and X2 restrict V to
  equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X1
  to be less than X2. Likewise, if V ever becomes known to equal NIL then a
  noticer attached to V restricts X1 to be greater than or equal to X2.

Restricting a real value X1 to be less than a real value X2 is performed by
attaching noticers to X1 and X2. The noticer attached to X1 continually
restricts the lower bound of X2 to be no lower than the upper bound of X1 if
X1 has an upper bound. The noticer attached to X2 continually restricts the
upper bound of X1 to be no higher than the lower bound of X2 if X2 has a lower
bound. Since these restrictions only guarantee that X1 be less than or equal
to X2, the constraint that X1 be strictly less than X2 is enforced by having
the noticers fail when both X1 and X2 become known to be equal.

Restricting a real value X1 to be greater than or equal to a real value X2 is
performed by an analogous set of noticers without this last equality check."
  (<v-internal x xs))

(defun <=v-internal (x xs)
  (if (null xs)
      t
      (andv (<=v2 x (first xs)) (<=v-internal (first xs) (rest xs)))))

(defun <=v (x &rest xs)
  "All arguments are constrained to be real. Returns T when called with one
argument. A call such as \(<=V X1 X2 ... Xn) with more than two arguments
behaves like a conjunction of two argument calls:

  \(ANDV \(<=V X1 X2) ... \(<=V Xi Xi+1) ... \(<=V Xn-1 Xn))

When called with two arguments, returns T if X1 is know to be less than or equal to X2
at the time of the call, NIL if X1 is known to be greater than X2, and otherwise a new
boolean variable V.

Values of V, X1, and X2 are mutually constrained:

 * V is equal to T iff X1 is known to be less than or equal to X2.

 * V is equal to NIL iff X2 is known to be greater than X2.

 * If V is known to be T, X1 is constrained to be less than or equal to X2.

 * If V is known to be NIL, X1 is constrained to be greater than X2."
  (<=v-internal x xs))

(defun >v-internal (x xs)
  (if (null xs)
      t
      (andv (<v2 (first xs) x) (>v-internal (first xs) (rest xs)))))

(defun >v (x &rest xs)
  "All arguments are constrained to be real. Returns T when called with one
argument. A call such as \(>V X1 X2 ... Xn) with more than two arguments
behaves like a conjunction of two argument calls:

  \(ANDV \(> X1 X2) ... \(> Xi Xi+1) ... \(> Xn-1 Xn))

When called with two arguments, returns T if X1 is know to be greater than X2
at the time of the call, NIL if X1 is known to be less than or equal to X2,
and otherwise a new boolean variable V.

Values of V, X1, and X2 are mutually constrained:

 * V is equal to T iff X1 is known to be greater than X2.

 * V is equal to NIL iff X2 is known to be less than or equal to X2.

 * If V is known to be T, X1 is constrained to be greater than X2.

 * If V is known to be NIL, X1 is constrained to be less than or equal to X2."
  (>v-internal x xs))

(defun >=v-internal (x xs)
  (if (null xs)
      t
      (andv (<=v2 (first xs) x) (>=v-internal (first xs) (rest xs)))))

(defun >=v (x &rest xs)
  "All arguments are constrained to be real. Returns T when called
with one argument. A call such as \(>=V X1 X2 ... Xn) with more than two
arguments behaves like a conjunction of two argument calls:

  \(ANDV \(>=V X1 X2) ... \(>=V Xi Xi+1) ... \(>=V Xn-1 Xn))

When called with two arguments, returns T if X1 is know to be greater than or
equal to X2 at the time of the call, NIL if X1 is known to be less than X2,
and otherwise a new boolean variable V.

Values of V, X1, and X2 are mutually constrained:

 * V is equal to T iff X1 is known to be greater than or equal to X2.

 * V is equal to NIL iff X2 is know to be less than X2.

 * If V is known to be T, X1 is constrained to be greater than or equal to X2.

 * If V is known to be NIL, X1 is constrained to be less than X2."
  (>=v-internal x xs))

(defun /=v-internal (x xs)
  (if (null xs)
      t
      (andv (/=v2 x (first xs))
            (/=v-internal x (rest xs))
            (/=v-internal (first xs) (rest xs)))))

(defun /=v (x &rest xs)
  "Returns a boolean value which is constrained to be T if no two arguments
are numerically equal, and constrained to be NIL if any two or more arguments
are numerically equal.

This function takes one or more arguments. All of the arguments are restricted
to be numeric.

Returns T when called with one argument. A call such as \(/=V X1 X2 ... Xn)
with more than two arguments behaves like a conjunction of two argument calls:

  \(ANDV \(/=V X1 X2) ... \(/=V X1 Xn)
        \(/=V X2 X3) ... \(/=V X2 Xn)
        ...
        \(/=V Xi Xi+1 ... \(/=V Xi Xn)
        ...
        \(/=V Xn-1 xn))

When called with two arguments, returns T if X1 is known not to be equal to X2
at the time of call, NIL if X1 is known to be equal to X2 at the time of
call, and otherwise a new boolean variable V.

Two numeric values are known not to be equal when their domains are disjoint.

Two real values are known not to be equal when their ranges are disjoint, i.e.
the upper bound of one is greater than the lower bound of the other.

Two numeric values are known to be equal only when they are both bound and
equal according to the Common Lisp function =.

When a new variable is created, the values of X1, X2 and V are mutually
constrained via noticers so that V is equal to T if and only if X1 is known
not to be equal to X2 and V is equal to NIL if and only if X1 is known to be
equal to X2.

* If it later becomes known that X1 is not equal to X2, noticers attached to
  X1 and X2 restrict V to equal T. Likewise, if it later becomes known that X1
  is equal to X2, noticers attached to X1 and X2 restrict V to equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X1
  to not be equal to X2. Likewise, if V ever becomes known to equal NIL then a
  noticer attached to V restricts X1 to be equal to X2.

Restricting two values X1 and X2 to be equal is performed by attaching
noticers to X1 and X2. These noticers continually restrict the domains of X1
and X2 to be equivalent sets \(using the Common Lisp function = as a test
function) as their domains are restricted. Furthermore, if X1 is known to be
real then the noticer attached to X2 continually restrict the upper bound of
X1 to be no higher than the upper bound of X2 and the lower bound of X1 to be
no lower than the lower bound of X2. The noticer of X2 performs a symmetric
restriction on the bounds of X1 if it is known to be real.

Restricting two values X1 and X2 to not be equal is also performed by
attaching noticers to X1 and X2. These noticers however, do not restrict the
domains or ranges of X1 or X2. They simply monitor their continually
restrictions and fail when any assertion causes X1 to be known to be equal to
X2."
  (/=v-internal x xs))

;;; == for as close to proper unification as Screamer currently supports
;;; Effectively a Screamer analogue of equalpv
(defun known?-==v2-variable (x y)
  (s:nest
   (or (equalp x y))
   (cond
     ((and (variable-real? x)
           (variable-real? y))
      (and (known?-<=v2 x y)
           (known?-<=v2 y x))))
   ((and (ground? x)
         (ground? y)))
   (let ((x (variable-value x))
         (y (variable-value y))))
   (cond ((and (consp x) (consp y))
          (and (known?-==v2 (car x) (car y))
               (known?-==v2 (cdr x) (cdr y))))
         ((and x y (s:sequencep x) (s:sequencep y)
               (= (length x) (length y)))
          (every #'known?-==v2 x y))
         (t (equalp x y)))))

(defun known?-/==v2-variable (x y)
  (let ((xv (value-of x))
        (yv (value-of y)))
    (cond ((and (variable-real? x)
                (variable-real? y))
           (or (known?-<v2-variable x y) (known?-<v2-variable y x)))
          ((and (consp xv) (consp yv))
           (and (known?-/==v2 (car xv) (car yv))
                (known?-/==v2 (cdr xv) (cdr yv))))
          ((and (s:sequencep xv) (s:sequencep yv)
                xv yv ;; Make sure it's not just them being nil
                )
           (or (not (= (length xv) (length yv)))
               (every #'known?-/==v2 xv yv)))
          ;;; TODO: Make this correct when dealing with e.g. structs
          ((and (bound? x) (bound? y)
                (not (equalp xv yv))) t))))

(defun known?-==v2-internal (x y)
  (known?-==v2-variable (variablize x) (variablize y)))

(defun known?-/==v2-internal (x y)
  (known?-/==v2-variable (variablize x) (variablize y)))

(defun known?-==v2 (x y)
  (known?-==v2-internal x y))

(defun known?-/==v2 (x y)
  (known?-/==v2-internal x y))

(defun known?-==v-internal (x xs)
  (if (null xs)
      t
      (and (known?-==v2 x (first xs))
           (known?-==v-internal (first xs) (rest xs)))))

(defun known?-==v (x &rest xs) (known?-==v-internal x xs))

(defun known?-/==v-internal (x xs)
  (if (null xs)
      t
      (and (known?-/==v2 x (first xs))
           (known?-/==v-internal x (rest xs))
           (known?-/==v-internal (first xs) (rest xs)))))

(defun known?-/==v (x &rest xs) (known?-/==v-internal x xs))

(defun ==-rule (x y)
  (when (known?-/==v2 x y) (fail))
  (when (and (variable-real? x) (variable-real? y))
    (restrict-bounds! x (variable-lower-bound y) (variable-upper-bound y))
    (restrict-bounds! y (variable-lower-bound x) (variable-upper-bound x)))
  (let ((xv (when (bound? x) (value-of x)))
        (yv (when (bound? y) (value-of y))))
    (s:nest
     (cond ((and (consp xv)
                 (consp yv))
            (==-rule (variablize (car xv))
                     (variablize (car yv)))
            (==-rule (variablize (cdr xv))
                     (variablize (cdr yv))))
           ((and (s:sequencep xv) (s:sequencep yv)
                 xv yv ;; Make sure it's not just them being nil
                 )
            (map nil (lambda (a b) (==-rule (variablize a)
                                            (variablize b)))
                 xv yv)))
     (t)
     (let ((xdom (cond
                   ((and (variable? x)
                         (subtypep (type-of (variable-enumerated-domain x)) 'list))
                    (variable-enumerated-domain x))
                   ((bound? x) (list (value-of x)))
                   (t nil)))
           (ydom (cond
                   ((and (variable? y)
                         (subtypep (type-of (variable-enumerated-domain y)) 'list))
                    (variable-enumerated-domain y))
                   ((bound? y) (list (value-of y)))
                   (t nil)))))
     (cond ((and (bound? y) (not xdom))
            (set-enumerated-domain! x ydom))
           ((and (bound? x) (not ydom))
            (set-enumerated-domain! y xdom)))
     (t)
     (when (and xdom ydom
                (or (variable? x) (variable? y))))
     (let ((joined (intersection xdom ydom :test #'equal))))
     (mapc (lambda (v)
             (when (variable? v)
               (restrict-enumerated-domain! v joined)))
           (list x y)))))

(defun /==-rule (x y)
  ;; NOTE: sequences are handled in assert!-/==v2
  (labels ((block-possible-value (var val)
             (let ((dom (variable-enumerated-domain var))
                   (antidom (variable-enumerated-antidomain var)))
               (if (subtypep (type-of (variable-enumerated-domain var)) 'list)
                   (when (member (value-of val) dom)
                     (restrict-enumerated-domain! var
                                                  (remove (value-of val) dom)))
                   (restrict-enumerated-antidomain! var
                                                    (cons (value-of val) antidom))))))
    (let ((xv (value-of x))
          (yv (value-of y)))
      (when (known?-==v2 x y) (fail))
      (when (and (bound? x)
                 (variable? y))
        (block-possible-value y xv))
      (when (and (bound? y)
                 (variable? x))
        (block-possible-value x yv)))))

(defun assert!-==v2 (x y)
  (let ((x (variablize x))
        (y (variablize y)))
    (attach-noticer! #'(lambda () (==-rule x y)) x)
    (attach-noticer! #'(lambda () (==-rule x y)) y)))

(defun assert!-==v-internal (x xs)
  (unless (null xs)
    (assert!-==v2 x (first xs))
    (assert!-==v-internal (first xs) (rest xs))))

(defun assert!-==v (x &rest xs) (assert!-==v-internal x xs))

(defun assert!-/==v2 (x y)
  (let ((xv (value-of x))
        (yv (value-of y)))
    (cond
      ((and (bound? x) (bound? y)
            (s:sequencep xv) (s:sequencep yv))
       (let ((known-mismatch nil)
             (a-variables nil)
             (b-variables nil))
         (iter:iter
           (iter:for a in-sequence xv)
           (iter:for b in-sequence yv)
           (if (or (not (bound? a))
                   (not (bound? b)))
               (progn
                 (push a a-variables)
                 (push b b-variables))
               (unless (equalp (value-of a) (value-of b))
                 (setf known-mismatch t)
                 (return nil))))
         (unless known-mismatch
           (assert! (notv (apply #'andv
                                 (mapcar #'==v
                                         a-variables
                                         b-variables)))))))
      (t (let ((x (variablize x))
               (y (variablize y)))
           (attach-noticer! #'(lambda () (/==-rule x y)) x)
           (attach-noticer! #'(lambda () (/==-rule x y)) y))))))

(defun assert!-/==v-internal (x xs)
  (unless (null xs)
    (assert!-/==v2 x (first xs))
    (assert!-/==v-internal (first xs) (rest xs))))

(defun assert!-/==v (x &rest xs) (assert!-/==v-internal x xs))

(defun ==v2 (x y)
  (cond ((known?-==v2-internal x y) t)
        ((known?-/==v2-internal x y) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-==v2-variable x y) (restrict-true! z))
                        ((known?-/==v2-variable x y) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-==v2-variable x y) (restrict-true! z))
                        ((known?-/==v2-variable x y) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-==v2 x y))
                        ((variable-false? z) (assert!-/==v2 x y))))
              z)
             z))))

(defun /==v2 (x y)
  (cond ((known?-/==v2-internal x y) t)
        ((known?-==v2-internal x y) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-/==v2-variable x y) (restrict-true! z))
                        ((known?-==v2-variable x y) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-/==v2-variable x y) (restrict-true! z))
                        ((known?-==v2-variable x y) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-/==v2 x y))
                        ((variable-false? z) (assert!-==v2 x y))))
              z)
             z))))

(defun ==v-internal (x xs)
  (if (null xs)
      t
      (andv (==v2 x (first xs))
            (==v-internal (first xs) (rest xs)))))

(defun ==v (x &rest xs)
  "An extension of `=v' to work on most atoms by using `equal' to check equality."
  (==v-internal x xs))

(defun /==v-internal (x xs)
  (if (null xs)
      t
      (andv (/==v2 x (first xs))
            (/==v-internal x (rest xs))
            (/==v-internal (first xs) (rest xs)))))

(defun /==v (x &rest xs)
  "The inverse of `==v'"
  (/==v-internal x xs))

(cl:defun == (a &rest xs)
  (iter:iter (iter:for x in xs)
    (when (not (equalp a x))
      (return nil))
    (iter:finally (return t))))
(cl:defun /== (a &rest xs) (not (apply #'== a xs)))

(cl:defun all-different (li &key (test #'equal))
  (if (null (cdr li))
      t
      (iter:iter
        (iter:for i in li)
        ;; Return nil if there is a duplicate
        (when (member i l :test test)
          (return nil))
        ;; Track previous values
        (iter:collect i into l)
        (iter:finally (return t)))))

(defun all-differentv (inp)
  "Functionally the same as (apply #'/=v inp), but faster.
Works on nested sequences which potentially contain variables, e.g. (all-differentv '((1 2) (2 3))."
  (let* ((val-diff-func (cond
                          ((every #'known?-numberpv inp)
                           #'/=v)
                          (t #'/==v)))
         (seq-diff-func (lambda (a b) (notv (equalv a b))))
         (diff-func (lambda (a b)
                      (funcall (if (or (subtypep (type-of a) 'sequence)
                                       (subtypep (type-of b) 'sequence))
                                   seq-diff-func
                                   val-diff-func)
                               a b))))
    (apply #'andv
           (mapcon
            (lambda (xs)
              (when (cdr xs)
                (mapcar
                 (curry diff-func (car xs))
                 (cdr xs))))
            (coerce inp 'list)))))

;;; Lifted EQUALV

(defun known?-equalv (x y)
  (or (eql x y)
      (cond ((variable? x)
             (and (not (eq (variable-value x) x))
                  (known?-equalv (variable-value x) y)))
            ((variable? y)
             (and (not (eq (variable-value y) y))
                  (known?-equalv x (variable-value y))))
            ((and (consp x) (consp y))
             (known?-equalv (car x) (car y))
             (known?-equalv (cdr x) (cdr y)))
            ((and (s:sequencep x) (s:sequencep y) (= (length x) (length y)))
             (every #'known?-equalv x y))
            ((and (arrayp x) (arrayp y) (equal (array-dimensions x) (array-dimensions y)))
             ;; If they have the same shape, equate each element
             (every (lambda (idx) (known?-equalv (row-major-aref x idx) (row-major-aref y idx)))
                    (iota (array-total-size x))))
            ((and (hash-table-p x) (hash-table-p y) (equal (hash-table-keys x) (hash-table-keys y)))
             ;; If they have the same keys, equate the value of each key
             (every (lambda (k) (known?-equalv (gethash k x) (gethash k y))) (hash-table-keys x)))
            (t (equal x y)))))

(defun assert!-equalv (x y)
  (unless (eql x y)
    (cond ((variable? x)
           (cond ((not (eq (variable-value x) x))
                  (assert!-equalv (variable-value x) y))
                 ((variable? y)
                  (if (eq (variable-value y) y)
                      (share! x y)
                      (assert!-equalv x (variable-value y))))
                 (t (restrict-value! x y))))
          ((variable? y)
           (if (eq (variable-value y) y)
               (restrict-value! y x)
               (assert!-equalv x (variable-value y))))
          ((and (consp x) (consp y))
           (assert!-equalv (car x) (car y))
           (assert!-equalv (cdr x) (cdr y)))
          ((and (s:sequencep x) (s:sequencep y) (= (length x) (length y)))
           (cl:map nil #'assert!-equalv x y))
          ((and (arrayp x) (arrayp y) (equal (array-dimensions x) (array-dimensions y)))
           ;; If they have the same shape, assert every element of each array to be equalv.
           (cl:map nil (lambda (idx) (assert!-equalv (row-major-aref x idx) (row-major-aref y idx))) (iota (array-total-size x))))
          ((and (hash-table-p x) (hash-table-p y) (equal (hash-table-keys x) (hash-table-keys y)))
           ;; If they have the same keys, assert every element of each hash table to be the same
           (cl:map nil (lambda (k) (assert!-equalv (gethash k x) (gethash k y))) (hash-table-keys x)))
          (t (fail)))))

(defun known?-notv-equalv (x y) (one-value (progn (assert!-equalv x y) nil) t))

(defun assert!-notv-equalv (x y)
  (cond
    ((known?-equalv x y) (fail))
    ((not (known?-notv-equalv x y))
     (let* ((x (variablize x))
            (y (variablize y))
            (noticer #'(lambda ()
                         (cond ((and (known?-numberpv x)
                                     (known?-numberpv y))
                                (/=-rule x y))
                               ((known?-equalv x y) (fail))))))
       (attach-noticer! noticer x)
       (attach-noticer! noticer y)))))

(defun equalv (x y)
  "Returns T if the aggregate object X is known to equal the aggregate object
Y, NIL if the aggregate object X is known not to equal the aggregate object Y,
and a new boolean variable V if it is not known whether or not X equals Y when
EQUALV is called.

The values of X, Y and V are mutually constraints via noticers so that V
equals T if and only if X is known to equal Y and V equals NIL if and only if
X is known not to equal Y.

Noticers are attached to V as well as to all variables nested in both in X and
Y. When the noticers attached to variables nested in X and Y detect that X is
known to equal Y they restrict V to equal T. Likewise, when the noticers
attached to variables nested in X and Y detect that X is known not to equal Y
they restrict V to equal NIL.

Furthermore, if V later becomes known to equal T then X and Y are unified.
Likewise, if V later becomes known to equal NIL then X and Y are restricted to
not be equal. This is accomplished by attaching noticers to the variables
nested in X and Y which detect when X becomes equal to Y and fail.

The expression \(KNOWN? (EQUALV X Y)) is analogous to the extra-logical predicate
`==' typically available in Prolog.

The expression \(KNOWN? (NOTV (EQUALV X Y))) is analogous to the extra-logical
predicate `\\=' typically available in Prolog.

The expression \(ASSERT! (EQUALV X Y)) is analogous to Prolog unification.

The expression \(ASSERT! (NOTV (EQUALV X Y))) is analogous to the
disunification operator available in Prolog-II."
  ;; NOTE: Can be made more efficient and return an AND tree of individual
  ;;       constraints needed to make EQUALV true. This can be done also for
  ;;       the KNOWN? and ASSERT! versions.
  (cond ((known?-equalv x y) t)
        ((known?-notv-equalv x y) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-equalv x y) (restrict-true! z))
                        ((known?-notv-equalv x y) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-equalv x y) (restrict-true! z))
                        ((known?-notv-equalv x y) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-equalv x y))
                        ((variable-false? z) (assert!-notv-equalv x y))))
              z)
             z))))

;;; The Optimizer Macros for ASSERT!, KNOWN? and DECIDE

(defun known?-true (x) (assert!-booleanpv x) (eq (value-of x) t))

(defun known?-false (x) (assert!-booleanpv x) (null (value-of x)))

(defun-compile-time transform-known? (form polarity?)
  (if (and (consp form) (null (rest (last form))))
      (cond
        ((and (eq (first form) 'notv)
              (= (length form) 2))
         (transform-known? (second form) (not polarity?)))
        ((eq (first form) 'andv)
         (cached-cons (if polarity? 'and 'or)
                      (mapcar #'(lambda (form) (transform-known? form polarity?))
                              (rest form))))
        ((eq (first form) 'orv)
         (cached-cons (if polarity? 'or 'and)
                      (mapcar #'(lambda (form) (transform-known? form polarity?))
                              (rest form))))
        ((member (first form)
                 '(integerpv realpv numberpv memberv booleanpv
                   =v <v <=v >v >=v /=v funcallv applyv equalv)
                 :test #'eq)
         (cached-cons (cdr (assoc (first form)
                                  (if polarity?
                                      '((integerpv . known?-integerpv)
                                        (realpv . known?-realpv)
                                        (numberpv . known?-numberpv)
                                        (memberv . known?-memberv)
                                        (booleanpv . known?-booleanpv)
                                        (=v . known?-=v)
                                        (<v . known?-<v)
                                        (<=v . known?-<=v)
                                        (>v . known?->v)
                                        (>=v . known?->=v)
                                        (/=v . known?-/=v)
                                        (funcallv . known?-funcallv)
                                        (applyv . known?-applyv)
                                        (equalv . known?-equalv))
                                      '((integerpv . known?-notv-integerpv)
                                        (realpv . known?-notv-realpv)
                                        (numberpv . known?-notv-numberpv)
                                        (memberv . known?-notv-memberv)
                                        (booleanpv . known?-notv-booleanpv)
                                        (=v . known?-/=v)
                                        (<v . known?->=v)
                                        (<=v . known?->v)
                                        (>v . known?-<=v)
                                        (>=v . known?-<v)
                                        (/=v . known?-=v)
                                        (funcallv . known?-notv-funcallv)
                                        (applyv . known?-notv-applyv)
                                        (equalv . known?-notv-equalv)))
                                  :test #'eq))
                      (rest form)))
        (polarity? `(known?-true ,form))
        (t `(known?-false ,form)))
      (if polarity? `(known?-true ,form) `(known?-false ,form))))

(defmacro-compile-time known? (x)
  "Restricts X to be a boolean. If X is equal to T after being restricted to
be boolean, returns T. If X is equal to NIL or if the value of X is unknown
returns NIL. The argument X can be either a variable or a non-variable.

The initial restriction to boolean may cause other assertions to be made due
to noticers attached to X. A call to KNOWN? fails if X is known not to be
boolean prior to the assertion or if any of the assertions performed by the
noticers result in failure.

Restricting X to be boolean attaches a noticer on X so that any subsequent
assertion which restricts X to be non-boolean will fail.

Except for the fact that one cannot write #'KNOWN?, KNOWN? behaves like a
function, even though it is implemented as a macro.

The reason it is implemented as a macro is to allow a number of compile time
optimizations. Expressions like \(KNOWN? \(NOTV X)), \(KNOWN? \(NUMBERPV X))
and \(KNOWN? \(NOTV \(NUMBERPV X))) are transformed into calls to functions
internal to Screamer which eliminate the need to create the boolean
variable\(s) normally returned by functions like NOTV and NUMBERV. Calls to
the functions NUMBERPV, REALPV, INTEGERPV, MEMBERV, BOOLEANPV, =V, <V, <=V, V,
>=v, /=v, NOTV, FUNCALLV, APPLYV and EQUALV which appear directly nested in a
call to KNOWN?, or directly nested in a call to NOTV which is in turn directly
nested in a call to KNOWN?, are similarly transformed."
  ;; FIXME: better done with a function & compiler-macro
  (transform-known? x t))

(defun assert!-true (x) (assert!-equalv x t))

(defun assert!-false (x) (assert!-equalv x nil))

(defun-compile-time transform-assert! (form polarity?)
  (if (and (consp form) (null (rest (last form))))
      (cond
        ((and (eq (first form) 'notv)
              (= (length form) 2))
         (transform-assert! (second form) (not polarity?)))
        ((eq (first form) 'andv)
         (if polarity?
             `(progn ,@(mapcar
                        #'(lambda (form) (transform-assert! form polarity?))
                        (rest form)))
             (cond ((null (rest form)) `(fail))
                   ((null (rest (rest form))) `(assert!-false ,(second form)))
                   (t `(assert!-notv-andv ,@(rest form))))))
        ((eq (first form) 'orv)
         (if polarity?
             (cond ((null (rest form)) `(fail))
                   ((null (rest (rest form))) `(assert!-true ,(second form)))
                   (t `(assert!-orv ,@(rest form))))
             `(progn ,@(mapcar
                        #'(lambda (form) (transform-assert! form polarity?))
                        (rest form)))))
        ((member (first form)
                 '(integerpv realpv numberpv memberv booleanpv
                   =v <v <=v >v >=v /=v funcallv applyv ==v /==v equalv)
                 :test #'eq)
         (cached-cons (cdr (assoc (first form)
                                  (if polarity?
                                      '((integerpv . assert!-integerpv)
                                        (realpv . assert!-realpv)
                                        (numberpv . assert!-numberpv)
                                        (memberv . assert!-memberv)
                                        (booleanpv . assert!-booleanpv)
                                        (=v . assert!-=v)
                                        (<v . assert!-<v)
                                        (<=v . assert!-<=v)
                                        (>v . assert!->v)
                                        (>=v . assert!->=v)
                                        (/=v . assert!-/=v)
                                        (funcallv . assert!-funcallv)
                                        (applyv . assert!-applyv)
                                        (==v . assert!-==v)
                                        (/==v . assert!-/==v)
                                        (equalv . assert!-equalv))
                                      '((integerpv . assert!-notv-integerpv)
                                        (realpv . assert!-notv-realpv)
                                        (numberpv . assert!-notv-numberpv)
                                        (memberv . assert!-notv-memberv)
                                        (booleanpv . assert!-notv-booleanpv)
                                        (=v . assert!-/=v)
                                        (<v . assert!->=v)
                                        (<=v . assert!->v)
                                        (>v . assert!-<=v)
                                        (>=v . assert!-<v)
                                        (/=v . assert!-=v)
                                        (funcallv . assert!-notv-funcallv)
                                        (applyv . assert!-notv-applyv)
                                        (==v . assert!-/==v)
                                        (/==v . assert!-==v)
                                        (equalv . assert!-notv-equalv)))
                                  :test #'eq))
                      (rest form)))
        (polarity? `(assert!-true ,form))
        (t `(assert!-false ,form)))
      (if polarity? `(assert!-true ,form) `(assert!-false ,form))))

(defmacro-compile-time assert! (x)
  "Restricts X to T. No meaningful result is returned. The argument X can be
either a variable or a non-variable.

This assertion may cause other assertions to be made due to noticers attached
to X.

A call to ASSERT! fails if X is known not to equal T prior to the assertion or
if any of the assertions performed by the noticers result in failure.

Except for the fact that one cannot write #'ASSERT!, ASSERT! behaves like a
function, even though it is implemented as a macro.

The reason it is implemented as a macro is to allow a number of compile time
optimizations. Expressions like \(ASSERT! \(NOTV X)), \(ASSERT! \(NUMBERPV X))
and \(ASSERT! \(NOTV \(NUMBERV X))) are transformed into calls to functions
internal to Screamer which eliminate the need to create the boolean
variable\(s) normally returned by functions like NOTV and NUMBERPV. Calls to
the functions NUMBERPV, REALPV, INTEGERPV, MEMBERV, BOOLEANPV, =V, <V, <=V,
>V, >=V, /=V, NOTV, FUNCALLV, APPLYV and EQUALV which appear directly nested
in a call to ASSERT!, or directly nested in a call to NOTV which is in turn
directly nested in a call to ASSERT!, are similarly transformed."
  ;; FIXME: Should probably be a function + a compiler macro.
  (transform-assert! x t))

(defun-compile-time transform-decide (form polarity?)
  (if (and (consp form) (null (rest (last form))))
      (cond
        ((and (eq (first form) 'notv)
              (= (length form) 2))
         (transform-decide (second form) (not polarity?)))
        ((eq (first form) 'andv)
         (let ((result (mapcar #'(lambda (form)
                                   (multiple-value-list
                                    (transform-decide form polarity?)))
                               (rest form))))
           (values (reduce #'append (mapcar #'first result))
                   (cached-cons (if polarity? 'progn 'either)
                                (mapcar #'second result))
                   (cached-cons (if polarity? 'either 'progn)
                                (mapcar #'third result)))))
        ((eq (first form) 'orv)
         (let ((result (mapcar #'(lambda (form)
                                   (multiple-value-list
                                    (transform-decide form polarity?)))
                               (rest form))))
           (values (reduce #'append (mapcar #'first result))
                   (cached-cons (if polarity? 'either 'progn)
                                (mapcar #'second result))
                   (cached-cons (if polarity? 'progn 'either)
                                (mapcar #'third result)))))
        ((member (first form)
                 '(integerpv realpv numberpv memberv booleanpv
                   =v <v <=v >v >=v /=v funcallv applyv ==v /==v equalv)
                 :test #'eq)
         (let ((arguments (mapcar #'(lambda (argument)
                                      (declare (ignore argument))
                                      (gensym "ARGUMENT-"))
                                  (rest form))))
           (values (mapcar #'list arguments (rest form))
                   (cached-cons (cdr (assoc (first form)
                                            (if polarity?
                                                '((integerpv . assert!-integerpv)
                                                  (realpv . assert!-realpv)
                                                  (numberpv . assert!-numberpv)
                                                  (memberv . assert!-memberv)
                                                  (booleanpv . assert!-booleanpv)
                                                  (=v . assert!-=v)
                                                  (<v . assert!-<v)
                                                  (<=v . assert!-<=v)
                                                  (>v . assert!->v)
                                                  (>=v . assert!->=v)
                                                  (/=v . assert!-/=v)
                                                  (funcallv . assert!-funcallv)
                                                  (applyv . assert!-applyv)
                                                  (==v . assert!-==v)
                                                  (/==v . assert!-/==v)
                                                  (equalv . assert!-equalv))
                                                '((integerpv . assert!-notv-integerpv)
                                                  (realpv . assert!-notv-realpv)
                                                  (numberpv . assert!-notv-numberpv)
                                                  (memberv . assert!-notv-memberv)
                                                  (booleanpv . assert!-notv-booleanpv)
                                                  (=v . assert!-/=v)
                                                  (<v . assert!->=v)
                                                  (<=v . assert!->v)
                                                  (>v . assert!-<=v)
                                                  (>=v . assert!-<v)
                                                  (/=v . assert!-=v)
                                                  (funcallv . assert!-notv-funcallv)
                                                  (applyv . assert!-notv-applyv)
                                                  (==v . assert!-/==v)
                                                  (/==v . assert!-==v)
                                                  (equalv . assert!-notv-equalv)))
                                            :test #'eq))
                                arguments)
                   (cached-cons (cdr (assoc (first form)
                                            (if polarity?
                                                '((integerpv . assert!-notv-integerpv)
                                                  (realpv . assert!-notv-realpv)
                                                  (numberpv . assert!-notv-numberpv)
                                                  (memberv . assert!-notv-memberv)
                                                  (booleanpv . assert!-notv-booleanpv)
                                                  (=v . assert!-/=v)
                                                  (<v . assert!->=v)
                                                  (<=v . assert!->v)
                                                  (>v . assert!-<=v)
                                                  (>=v . assert!-<v)
                                                  (/=v . assert!-=v)
                                                  (funcallv . assert!-notv-funcallv)
                                                  (applyv . assert!-notv-applyv)
                                                  (equalv . assert!-notv-equalv))
                                                '((integerpv . assert!-integerpv)
                                                  (realpv . assert!-realpv)
                                                  (numberpv . assert!-numberpv)
                                                  (memberv . assert!-memberv)
                                                  (booleanpv . assert!-booleanpv)
                                                  (=v . assert!-=v)
                                                  (<v . assert!-<v)
                                                  (<=v . assert!-<=v)
                                                  (>v . assert!->v)
                                                  (>=v . assert!->=v)
                                                  (/=v . assert!-/=v)
                                                  (funcallv . assert!-funcallv)
                                                  (applyv . assert!-applyv)
                                                  (equalv . assert!-equalv)))
                                            :test #'eq))
                                arguments))))
        (t (let ((argument (gensym "ARGUMENT-")))
             (values (list (list argument form))
                     (if polarity?
                         `(assert!-true ,argument)
                         `(assert!-false ,argument))
                     (if polarity?
                         `(assert!-false ,argument)
                         `(assert!-true ,argument))))))
      (let ((argument (gensym "ARGUMENT-")))
        (values
         (list (list argument form))
         (if polarity? `(assert!-true ,argument) `(assert!-false ,argument))
         (if polarity? `(assert!-false ,argument) `(assert!-true ,argument))))))

(defmacro-compile-time decide (x)
  "Restricts X to a be boolean. After X is restricted a nondeterministic
choice is made. For one branch, X is restricted to equal T and \(DECIDE X)
returns T as a result. For the other branch, X is restricted to equal NIL and
\(DECIDE X) returns NIL as a result. The argument X can be either a variable
or a non-variable.

The initial restriction to boolean may cause other assertions to be made due
to noticers attached to X. A call to DECIDE immediately fails if X is known
not to be boolean prior to the assertion or if any of the assertions performed
by the noticers result in failure.

Restricting X to be boolean attaches a noticer on X so that any subsequent
assertion which restricts X to be non-boolean will fail.

Except for implementation optimizations \(DECIDE X) is equivalent to:

  \(EITHER \(PROGN \(ASSERT! X) T) \(PROGN \(ASSERT! \(NOTV X)) NIL))

Except for the fact that one cannot write #'DECIDE, DECIDE behaves like a
function, even though it is implemented as a macro.

The reason it is implemented as a macro is to allow a number of compile time
optimizations. Expressions like \(DECIDE \(NOTV X)), \(DECIDE \(NUMBERPV X))
and \(DECIDE \(NOTV \(NUMBERPV X))) are transformed into calls to functions
internal to Screamer which eliminate the need to create the boolean
variable\(s) normally returned by functions like notv and numberv. Calls to
the functions NUMBERPV, REALPV, INTEGERPV, MEMBERPV, BOOLEANPV, =V, <V, <=V,
>V, >=V, /=V, NOTV, FUNCALLV, APPLYV and EQUALV which appear directly nested
in a call to decide, or directly nested in a call to NOTV which is in turn
directly nested in a call to decide, are similarly transformed."
  ;; FIXME: Sounds like this should be a function + compiler-macro.
  (cl:multiple-value-bind (arguments true false)
      (transform-decide x t)
    `(let ,arguments
       (either (progn ,true t) (progn ,false nil)))))

;;; Lifted Generators
;;; NOTE: The following functions could be handled more efficiently as special
;;;       cases.

(defun a-booleanv (&optional (name nil name?))
  "Returns a boolean variable."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (booleanpv v))
    v))

(defun an-integerv (&optional (name nil name?))
  "Returns an integer variable."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (integerpv v))
    v))

(defun an-integer-abovev (low &optional (name nil name?))
  "Returns an integer variable whose value is constrained to be greater than
or equal to LOW."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (integerpv v) (>=v v low)))
    v))

(defun an-integer-belowv (high &optional (name nil name?))
  "Returns an integer variable whose value is constrained to be less than or
equal to HIGH."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (integerpv v) (<=v v high)))
    v))

(defun an-integer-betweenv (low high &optional (name nil name?))
  "Returns an integer variable whose value is constrained to be greater than
or equal to LOW and less than or equal to HIGH. If the resulting integer
variable is bound, its value is returned instead. Fails if it is known that
there is no integer between LOW and HIGH at the time of call.

The expression \(AN-INTEGER-BETWEENV LOW HIGH) is an abbreviation for:

 \(LET ((V (MAKE-VARIABLE)))
    \(ASSERT! (INTEGERPV V))
    \(ASSERT! (>=V V LOW))
    \(ASSERT! (<=V V HIGH))
    \(VALUE-OF v))
"
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (integerpv v) (>=v v low) (<=v v high)))
    (value-of v)))

(defmacro let-integers-betweenv (((min max) var-list) &rest body)
  "Defines multiple logic variables with numerical values between min and max (in the same manner as with an-integer-betweenv).
Duplicate variable names will be ignored."
  `(let ,(loop for i in (remove-duplicates var-list)
               collect (list i `(an-integer-betweenv ,min ,max)))
     ,@body))

(defun a-realv (&optional (name nil name?))
  "Returns a real variable."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (realpv v))
    v))

(defun a-real-abovev (low &optional (name nil name?))
  "Returns a real variable whose value is constrained to be greater than or equal to LOW."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (realpv v) (>=v v low)))
    v))

(defun a-real-belowv (high &optional (name nil name?))
  "Returns a real variable whose value is constrained to be less than or equal to HIGH."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (realpv v) (<=v v high)))
    v))

(defun a-real-betweenv (low high &optional (name nil name?))
  "Returns a real variable whose value is constrained to be greater than or
equal to low and less than or equal to high. If the resulting real variable is
bound, its value is returned instead. Fails if it is known that low is greater
than high at the time of call.

The expression \(A-REAL-BETWEENV LOW HIGH) is an abbreviation for:

 \(LET ((V (MAKE-VARIABLE)))
    \(ASSERT! (REALPV V))
    \(ASSERT! (>=V V LOW))
    \(ASSERT! (<=V V HIGH))
    \(VALUE-OF V))
"
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (realpv v) (>=v v low) (<=v v high)))
    v))

(defun a-numberv (&optional (name nil name?))
  "Returns a variable whose value is constained to be a number."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (numberpv v))
    v))

(defun a-member-ofv (values &optional (name nil name?))
  "Returns a variable whose value is constrained to be one of VALUES.
VALUES can be either a vector or a list designator."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (memberv v values))
    (value-of v)))

;;; Search Control

(cl:defun get-variable-dependency-closure (variables &key (complete nil))
  (declare (list variables))
  (setf variables (remove-duplicates variables))
  (let ((new-vars nil)
        (deps nil))
    ;; NOTE: This is a kludge to avoid `loop', which
    ;; some compilers expand to MACROLETs
    (do* ((curr-vars variables new-vars))
         ;; When there are no variables to get dependencies of, leave
         ((not curr-vars) variables)
      ;; Get the dependencies of curr-vars
      (setf deps (mappend (lambda (v)
                            ;; Collect the dependencies
                            (when (variable? v)
                              (let ((bounded-var (bounded? v))
                                    (v-deps (variable-dependencies v)))
                                (if (and bounded-var (not complete))
                                    (progn
                                      ;; If v is bounded, only add bounded dependencies,
                                      ;; to minimize infinite loops
                                      (setf v-deps (remove-if-not #'bounded? v-deps))
                                      ;; Return dependencies only if they cumulatively
                                      ;; have fewer possibilities than the enumerated
                                      ;; domain of `v'.
                                      (unless (and (serapeum:sequencep (variable-enumerated-domain v))
                                                   (< (length (variable-enumerated-domain v))
                                                      (reduce #'* v-deps
                                                              :key (serapeum:op
                                                                     (let ((dom (variable-enumerated-domain _)))
                                                                       (etypecase dom (list (length dom)) (t 1)))))))
                                        v-deps))
                                    ;; If asked for the complete closure or v is not
                                    ;; known to be bounded, return all dependencies
                                    v-deps))))
                          curr-vars))
      ;; Filter out dependencies that are already tracked
      ;; NOTE: Kludged
      (setf new-vars (set-difference deps variables))
      ;; Sort new dependencies to force bounded variables first
      (setf new-vars (sort new-vars (lambda (a b) (and (bounded? a) (not (bounded? b))))))
      ;; Add each layer of dependencies to the start of the variable list
      (setf variables (append new-vars variables))))
  ;; Remove any undesired elements creeping in from mistakes in variable
  ;; initialization
  (setf variables (remove-if-not #'variable? (remove-duplicates variables)))
  ;; Move any unbounded variables to the end of the forcing list
  ;; in case they can be constrained by forcing the other variables.
  ;; FIXME: This requires perfect type inference, otherwise we may
  ;; force a value early which can't be fulfilled by the unbounded
  ;; variables.
  ;; (sort variables (lambda (a b) (and (not (bounded? b)) (bounded? a))))
  )

(defun variables-in (x)
  ;; Get initial variable list from `x'
  (the list
       (typecase x
         (cons (append (variables-in (car x))
                       (variables-in (cdr x))))
         (string nil)
         ;; (simple-vector (apply #'append (map 'list #'variables-in x)))
         (sequence (apply #'append (map 'list #'variables-in x)))
         (array (flet ((mappend-arr (arr f)
                         (let (coll)
                           (dotimes (idx (array-total-size arr))
                             (appendf coll (funcall f (row-major-aref arr idx))))
                           coll)))
                  (mappend-arr x #'variables-in)))
         (hash-table (let (coll)
                       (maphash (lambda (k v)
                                  (declare (ignore k))
                                  (appendf coll (variables-in v)))
                                x)
                       coll))
         (variable (list x))
         (otherwise nil))))

;;; NOTE: SOLUTION and LINEAR-FORCE used to be here but was moved to be before
;;;       KNOWN?-CONSTRAINT to avoid forward references to nondeterministic
;;;       functions.

(defun divide-and-conquer-force (variable)
  "Returns X if X is not a variable. If X is a bound variable then returns its
value. Otherwise implements a single binary-branching step of a
divide-and-conquer search algorithm. There are always two alternatives, the
second of which is tried upon backtracking.

If X is known to have a finite domain D then this domain is split into two
halves and the value of X is nondeterministically restricted to be a member
one of the halves. If X becomes bound by this restriction then its value is
returned. Otherwise, X itself is returned.

If X is not known to have a finite domain but is known to be real and to have
both lower and upper bounds:
If X could be an integer, the case where it is and isn't are both checked.
If X is a noninteger, enumerations are nondeterministically generated and tried
based on `*maximum-discretization-range*'. Then, nondeterministically either the
lower or upper bound is restricted to the midpoint between the lower and upper
bound. If X becomes bound by this restriction then its dereferenced value is
returned. Otherwise, X itself is returned.

An error is signalled if X is not known to be restricted to a finite domain
and either is not known to be real or is not known to have both a lower and
upper bound.

When the set of potential values may be infinite, users of
DIVIDE-AND-CONQUER-FORCE may need to take care to fail when the range size of
the variable becomes too small, unless other constraints on it are sufficient
to guarentee failure.

The method of splitting the domain into two halves is left unspecified to give
future implementations leeway in incorporating heuristics in the process of
determining a good search order. All that is specified is that if the domain
size is even prior to splitting, the halves are of equal size, while if the
domain size is odd, the halves differ in size by at most one."
  (let ((variable (value-of variable)))
    (if (variable? variable)
        (cond
          ((not (eq (variable-enumerated-domain variable) t))
           (let ((n (floor (length (variable-enumerated-domain variable)) 2)))
             (set-enumerated-domain!
              variable
              (either (subseq (variable-enumerated-domain variable) 0 n)
                (subseq (variable-enumerated-domain variable) n)))
             (run-noticers variable)))
          ((and (variable-real? variable)
                (variable-lower-bound variable)
                (variable-upper-bound variable))
           (if (variable-integer? variable)
               ;; Variable is known to be an integer with a bounded domain
               (let ((midpoint (floor (+ (variable-lower-bound variable)
                                         (variable-upper-bound variable))
                                      2)))
                 ;; Bisect the domain of the integer
                 (either (let ((old-bound (variable-upper-bound variable)))
                           (restrict-upper-bound! variable midpoint)
                           (if (= old-bound (variable-upper-bound variable))
                               (fail)))
                   (let ((old-bound (variable-lower-bound variable)))
                     (restrict-lower-bound! variable (1+ midpoint))
                     (if (= old-bound (variable-lower-bound variable))
                         (fail)))))
               ;; `variable' is not known to be an integer but is real and
               ;; has a bounded domain
               (let ((upper-bound (variable-upper-bound variable))
                     (lower-bound (variable-lower-bound variable)))
                 (let ((midpoint (/ (+ upper-bound lower-bound) 2)))
                   ;; Check various heuristics before deciding to bifurcate
                   (cond
                     ;; When `variable' might be an integer, try restricting
                     ;; it to an integer.
                     ((and
                       ;; If `variable' can't be an integer,
                       ;; don't bother checking if it can be
                       (variable-possibly-integer? variable)
                       (numberp upper-bound)
                       (numberp lower-bound)
                       ;; There is at least one integer between
                       ;; the bounds
                       (/= (floor upper-bound) (floor lower-bound)))
                      (either
                        ;; Try making `variable' an integer
                        (restrict-integer! variable)
                        ;; Look at non-integer values of `variable'
                        (restrict-noninteger! variable)))
                     ;; No heuristics apply
                     (t
                      ;; When a noninteger with no enumerated domain, try
                      ;; potential enumerations within the range before splitting
                      (when (not (zerop (range-size variable)))
                        (let* ((enumerations (when (and (numberp *maximum-discretization-range*)
                                                        (> *maximum-discretization-range* 0))
                                               (serapeum:range lower-bound upper-bound
                                                               (/ (range-size variable)
                                                                  *maximum-discretization-range*))))
                               ;; Add midpoint and bounds to make sure they're not missing.
                               ;; Also convert to a list for compatibility with function
                               ;; signatures.
                               ;; NOTE: If enumerations is empty then this just checks the bounds
                               (enumerations (concatenate 'list enumerations
                                                          (list lower-bound midpoint upper-bound)))
                               ;; Remove duplicate values
                               (enumerations (remove-duplicates enumerations :test #'=))
                               ;; Sort enumerations
                               (enumerations (cl:sort enumerations #'<)))
                          (either
                            (restrict-value! variable
                                             (a-member-of enumerations))
                            ;; If none of the enumerations work, restrict to be none of them
                            (restrict-enumerated-antidomain! variable enumerations))))
                      ;; Bifurfacte the domain unless `variable' is `bound?'
                      (unless (bound? variable)
                        (either
                          (let ((old-bound (variable-upper-bound variable)))
                            (restrict-upper-bound! variable midpoint)
                            (if (= old-bound (variable-upper-bound variable))
                                (fail)))
                          (let ((old-bound (variable-lower-bound variable)))
                            (restrict-lower-bound! variable midpoint)
                            (if (= old-bound (variable-lower-bound variable))
                                (fail)))))))))))
          (t (error "It is only possible to divide and conquer force a~%~
                  variable that has a countable domain or a finite range")))))
  (value-of variable))

;;; NOTE: STATIC-ORDERING used to be here but was moved to be before
;;;       KNOWN?-CONSTRAINT to avoid a forward reference to a nondeterministic
;;;       function.

(defun domain-size (x)
  "Returns the domain size of X.

If X is an integer variable with an upper and lower bound, its domain size
is the one greater than the difference of its bounds. Eg. [integer 1:2] has
domain size 2.

If X is a variable with an enumerated domain, its domain size is the size of
that domain.

If X is a CONS, or a variable whose value is a CONS, its domain size is the
product of the domain sizes of its CAR and CDR.

Other types of unbound variables have domain size NIL, whereas non-variables
have domain size of 1."
  (declare (optimize (speed 3) (space 3)))
  (let ((x (value-of x)))
    (typecase x
      (cons (infinity-* (domain-size (car x)) (domain-size (cdr x))))
      (variable
       (cond ((not (eq (variable-enumerated-domain x) t))
              (length (variable-enumerated-domain x)))
             ((and (variable-lower-bound x)
                   (variable-upper-bound x)
                   (variable-integer? x))
              (1+ (floor (- (variable-upper-bound x) (variable-lower-bound x)))))
             (t nil)))
      (otherwise 1))))

(defun range-size (x)
  "Returns the range size of X. Range size is the size of the range values
of X may take.

If X is an integer or a bound variable whose value is an integer, it has the
range size 0. Reals and bound variables whose values are reals have range size
0.0.

Unbound variables known to be reals with an upper and lower bound have a range
size the difference of their upper and lower bounds.

Other types of objects and variables have range size NIL."
  (let ((x (value-of x)))
    (typecase x
      (integer 0)
      (real 0.0)
      (variable (and (variable-real? x)
                     (variable-lower-bound x)
                     (variable-upper-bound x)
                     (- (variable-upper-bound x) (variable-lower-bound x))))
      (otherwise nil))))

(defun corrupted? (variable)
  (declare
   (optimize (speed 3) (space 3))
   (variable variable))
  (let* ((lower-bound (variable-lower-bound variable))
         (upper-bound (variable-upper-bound variable)))
    (and lower-bound
         upper-bound
         (/= lower-bound upper-bound)
         (let ((midpoint (/ (+ lower-bound upper-bound) 2)))
           (or (roughly-= midpoint lower-bound)
               (roughly-= midpoint upper-bound))))))

(defun find-best (cost order list)
  (let ((best nil)
        (best-cost nil))
    (dolist (x list)
      (let ((x (value-of x)))
        (if (and (variable? x) (not (corrupted? x)))
            (let ((cost (funcall cost x)))
              (when (and (not (null cost))
                         (or (null best-cost) (funcall order cost best-cost)))
                (setf best x)
                (setf best-cost cost))))))
    best))

(defun reorder-internal
    (variables cost-function terminate? order force-function)
  (let ((variable (find-best cost-function order variables)))
    (when (and variable
               (not (funcall terminate? (funcall cost-function variable))))
      (funcall-nondeterministic force-function (value-of variable))
      (reorder-internal
       variables cost-function terminate? order force-function))))

(defun reorder (cost-function terminate? order force-function)
  "Returns an ordering force function based on arguments.

The FORCE-FUNCTION is any (potentially nondeterministic) function
which can be applied to a variable as its single argument with the
stipulation that a finite number of repeated applications will force
the variable to be bound. The FORCE-FUNCTION need not return any useful value.

The ordering force function which is returned is a nondeterministic function
which takes a single argument X. This argument X can be a list of values where
each value may be either a variable or a non-variable.

The ordering force function repeatedly selects a \"best\" variable using using
COST-FUNCTION and ORDER. Eg. using #'DOMAIN-SIZE and #'< as the COST-FUNCTION
and ORDER, then the variable with the smallest domain will be forced first.

Function TERMINATE? is then called with the determined cost of that variable,
and unless it returns true, FORCE-FUNCTION is applied to that variable to
force constrain it.

Process then iterates until all variables become bound or TERMINATE? returns
true.

The ordering force function does not return any meaningful result.

Screamer currently provides two convenient force-functions, namely
#'linear-force and #'divide-and-conquer-force though future implementations
may provide additional ones. \(The defined Screamer protocol does not provide
sufficient hooks for the user to define her own force functions.)"
  ;; NOTE: This closure will heap cons.
  (let ((cost-function (value-of cost-function))
        (terminate? (value-of terminate?))
        (order (value-of order))
        (force-function (value-of force-function)))
    #'(lambda (variables)
        (setf variables (get-variable-dependency-closure variables))
        (reorder-internal
         variables cost-function terminate? order force-function))))

;;; FIXME: This doesn't make any sense. See branch "maybe" for an alternative
;;; expression. Also: why are we trying to increase the upper bound, and not
;;; the lower bound? Should the API also not allow us to minimize a variable
;;; towards either zero or negative infinity? --ns 2011-11-01
(defmacro-compile-time best-value
    (form1 objective-form &optional (form2 nil form2?))
  "First evaluates OBJECTIVE-FORM, which should evaluate to constraint variable V.

Then repeatedly evaluates FORM1 in non-deterministic context till it fails. If
previous round of evaluation produced an upper bound B for V, the during the
next round any change to V must provide an upper bound higher than B, or that
that change fails.

If the last successful evaluation of FORM produced an upper bound for V,
returns a list of two elements: the the primary value of FORM1 from that
round, and the upper bound of V.

Otherwise if FORM2 is provided, returns the result of evaluating it, or else
calls fails.

Note: this documentation string is entirely reverse-engineered. Lacking
information on just how BEST-VALUE was intended to work, it is hard to tell
what is a bug, an accident of implementation, and what is a feature. If you
have any insight into BEST-VALUE, please send email to
nikodemus@random-state.net."
  (let ((bound (gensym "BOUND-"))
        (best (gensym "BEST-"))
        (objective (gensym "OBJECTIVE-")))
    `(let ((,bound nil)
           (,best nil)
           (,objective (variablize ,objective-form)))
       (attach-noticer!
        #'(lambda ()
            (let ((upper (variable-upper-bound ,objective)))
              (when (and ,bound upper (<= upper ,bound))
                (fail))))
        ,objective)
       (for-effects
         (let ((value ,form1))
           (global
             (setf ,bound (variable-upper-bound ,objective))
             (setf ,best value))))
       (if ,bound (list ,best ,bound) ,(if form2? form2 '(fail))))))

(defun template-internal (template variables)
  (cond
    ((and (symbolp template) (char= #\? (aref (string template) 0)))
     (let ((binding (assoc template variables :test #'eq)))
       (if binding
           (values (cdr binding) variables)
           (let ((variable (make-variable template)))
             (values variable (cons (cons template variable) variables))))))
    ((consp template)
     (cl:multiple-value-bind (car-template car-variables)
         (template-internal (car template) variables)
       (cl:multiple-value-bind (cdr-template cdr-variables)
           (template-internal (cdr template) car-variables)
         (values (cons car-template cdr-template) cdr-variables))))
    (t (values template variables))))

(defun template (template)
  "Copies an aggregate object, replacing any symbol beginning with a question
mark with a newly created variable.

If the same symbol appears more than once in X, only one variable is created
for that symbol, the same variable replacing any occurrences of that symbol.
Thus \(TEMPLATE '(A B (?C D ?E) ?E)) has the same effect as:

  \(LET ((?C (MAKE-VARIABLE))
        \(?E (MAKE-VARIABLE)))
    \(LIST 'A 'B (LIST C 'D E) E)).

This is useful for creating patterns to be unified with other structures."
  (template-internal (value-of template) '()))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *screamer?* nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :screamer *features* :test #'eq))

;;; TODO: Fix the below examples:

;; NOTE: Seems to have something to do with integer bounds not propagating
;; properly when one of the values wasn't initially known to be an integer?
;; NOTE: This occurs because the sum of the first 4 elements of b is an
;; integer without a finite range. We don't track which atoms a variable
;; is dependent on and what its relationship is to them, so we aren't able
;; to recognize that (+ (+ 1 2 a 4) a) is the same as (+ 1 2 4 (* 2 a))
;; NOTE: How to resolve this? Figure out how to track atoms and use that
;; somehow? Do a compilation pass of the variable graph in `solution'
;; before trying to actually solve it?
;; NOTE: Using flow-cl here would be pretty useful... Organize the
;; current state as a standalone library and publish it?
;; NOTE: I've mitigated this by merging equivalent values in the
;; arglist of +v, but replacing the second ?a with ?c still causes
;; this issue
;; NOTE: I think we're forced to change the integer-bounds specification
;; framework entirely and implement a compilation pass in `solution' which
;; identifies unbound but related terms in a tree and merges them into a new
;; set of potential values
;; Example:
(serapeum:comment
  (all-values
    (let* ((b (template '(1 2 ?a 4 ?a)))
           (c (applyv #'+v b)))
      (print b)
      (print c)
      (assert! (integerpv (third b)))
      (assert! (=v (an-integer-betweenv 1 3) c))
      (assert! (integerpv c))
      (print b)
      (print c)
      (solution (list b c) (static-ordering #'linear-force)))))

;;; FIXME: There are memory faults somewhere in the screamer code
;;; If you use a `solution' form to wrap conses at the terminal position,
;;; then they get collected properly. However, if you just return
;;; them directly, components of them end up getting replaced.
;;;
;;; Replacing the `*last-value-cons*' code with a simple append fails
;;; to fix the issue in `all-values', while wrapping the body in `global'
;;; seems to at least succeed without weird replacement values, so this
;;; may be a bug in the CPS code for mutation, mutating a component of
;;; the collected final results when it shouldn't.
;;;
;;; NOTE: Confirmed that using `copy-list' at the end of an `all-values'
;;; form fixes the issue.
;;; NOTE: Mitigated for now by using `copy-tree' for any `consp' results
;;; in aggregation forms.
;;;
;;; FIXME: There is a bug somewhere in the assert-not code which causes
;;; failures even though a variable has valid values in its enumerated
;;; domain.
;;; NOTE: Presumably this would go away automatically once we've replaced
;;; noticers with types?
;;; Example:
(serapeum:comment
  (all-values
    (let* ((b (template '(1 2 ?a ?a 4)))
           (c (applyv #'+v b)))
      (assert! (integerpv (third b)))
      (assert! (<=v 1 c 3))
      (assert! (integerpv c))
      (print c)
      ;; This unilaterally fails
      (assert! (not (=v c 2)))
      (print c)
      (solution (list c b) (static-ordering #'linear-force)))))
;;; FIXME: Some macroexpansion cases seem to be failing since the
;;; CPS code can't handle cases that the walker can. Not sure if the
;;; problem is the disparity itself or an issue in the walker that
;;; keeps it from properly processing these forms
;;; Example:
(serapeum:comment
  (all-values
    (local
      (let* ((b (an-integer-between 1 2)) c)
        (iterate toplevel (for i from 1 to 2)
          (setf c (screamer::pure-values (b) b))
          (iterate (for j from 1 to 2)
            (in toplevel
                (collect (* (screamer::pure-one-value (i j)
                              (min i j))
                            c)))))))))

;; FIXME: If collector forms like ALL-VALUES use a gensym for `value',
;; some forms get the actual value put in the let binding-variable
;; slot, rather than a gensym.
;; Not sure what is making this error, though it seems to precede the
;; introduction of this gensym (which was itself done to fix memory
;; fault behaviors resulting from the output not being copied when it
;; was a list)
;; NOTE: So far I've only seen this specifically when a numeric
;; literal is placed in the final position
;; NOTE: Mitigated for now by using a bare unexported symbol instead
;; of a gensym, since in practice it's extremely unlikely to be clobbered
;; unless people either code in the `:screamer' package or intentionally
;; try to clobber it.
;; Example:
(serapeum:comment
  "failing case"
  (all-values (print 'hi) (either 2 3) 4)
  "similar succeeding case"
  (let ((real-value 5)) (all-values (print 'hi) (either 2 3) real-value)))

;;; TODO: Seeing some strange behavior around iteration and
;;; mutation, not yet sure what the dividing line is between
;;; working and incorrect
(serapeum:comment
  "returns NIL even though the (print coll) works fine"
  (let ((coll nil))
    (all-values
      (let ((coll nil))
        (local
          (iter (for i below 4)
            (for x = (an-integer-between 1 5))
            (when coll (assert! (necessarily? (> x (a-member-of coll)))))
            (push x coll)))
        (print coll))))
  "returning wrong values"
  (let ((coll nil))
    (all-values
      (let ((coll (local
                    (iter (for i below 2)
                      (for x = (an-integer-between 1 3))
                      (when coll (assert! (necessarily? (> x (a-member-of coll)))))
                      (collect x)))))
        coll)))
  "returning wrong values"
  (all-values
    (let ((coll (local
                  (iter (for i below 2)
                    (for prev initially 0 then i)
                    (for x = (an-integer-between 1 3))
                    (assert! (> x i))
                    (collect x)))))
      (assert! (all-different coll))
      coll))
  "Note: The above may just be failing due to bad code, the below works fine"
  (all-values
    (local
      (let ((coll (iter
                    (for i below 10)
                    (for prev initially 1 then x)
                    (for started initially nil then t)
                    (for x = (an-integer-between prev 11))
                    (when started (assert! (> x prev)))
                    (collect x))))
        (assert! (all-different coll))
        coll)))
  "The below seems pretty clearly wrong when (assert! (> x prev)) is removed though..."
  (let ((result-count 5))
    (all-values
      (local
        (iter
          (for i below (1- result-count))
          (for prev initially 0 then x)
          (for x = (an-integer-between (1+ prev) (min result-count (+ 2 prev))))
          (assert! (> x prev))
          (collect x))))))
