(in-package :screamer)

;;; TODO: Make the option to continue the continuation more ergonomic?
;;; Maybe make a separate helper which acts identically save for
;;; calling the continuation by default?
;;; FIXME: For forms like `one-value' and `n-values' that use a
;;; `tagbody' under the hood, invoking the `go' form (which is inevitable
;;; if you continue the continuation) leads to an error. This makes
;;; `call/cc' incompatible with such forms.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'call/cc))
(cl:defun call/cc (f)
  "EXPERIMENTAL
Call function F with the current continuation as input. Whether
execution of the continuation continues depends on the behavior of F.

F is assumed to be either a CL function or a deterministic Screamer
function.
NOTE: Is this assumption necessary? Can we make nondeterministic `call/cc's?

F is a single-argument function which is provided a function
(call it `cc' for convenience) which represents the current
continuation in the nondeterministic context.

`cc' is a 1-argument function which takes in a value, representing
the return value of a form. It then continues execution from the
current point of the nondeterministic context using that return
value.

If F does not call `cc', the current nondeterministic path is abandoned,
backtracking to the previous choice-point.

Calling `cc' results in continuing the saved nondeterministic path. If
done from within F itself, this will continue the current nondeterministic
context.
`cc' can be called from outside F, even outside nondeterministic context.
In this case it will continue its nondeterministic execution, but *will
not backtrack to choice points before the continuation was captured!!!*

Forms which populate the `*screamer-results*' form will do so globally
in the called continuation, unless the call is wrapped in a lexical
closure that closes over this variable (in which case Common Lisp's
standard behavior for dynamic bindings takes precedence).

`call/cc' does not currently work in `one-value' forms.

Example code:
(let* (cc-list
       (f (lambda (cc-internal)
            (push cc-internal cc-list)
            (print \"calling\")
            (print cc-list)
            (choice-point (funcall cc-internal nil)))))
  (print
   (all-values
     (print \"start\")
     (let ((x (either 1 2 3)))
       (print \"choice\")
       (print (call/cc f))
       (print \"called\")
       (print x))))
  (print \"exit\")
  (let (*screamer-results*)
    (mapcar (lambda (cc) (choice-point (funcall cc nil))) cc-list)
    (print *screamer-results*)))"
  (declare (ignorable f))
  (screamer-error
   "CALL/CC is a nondeterministic function. As such, it must be called only~%~
   from a nondeterministic context."))
(cl:defun call/cc-nondeterministic (continuation f)
  (let* ((stored-prob (current-probability))
         (cont (lambda (inp)
                 ;; Fix nondeterministic result accumulation by setting
                 ;; `*last-value-cons*' so we don't crash if
                 ;; `*screamer-results*' is non-`nil' before the result-
                 ;; accumulator the continuation comes from (assuming it is
                 ;; from one).
                 ;; TODO: Figure out if this can be done more elegantly
                 ;; than by making a new internal special variable
                 ;; just to track the ending of `*screamer-results*'
                 (let ((*last-value-cons* (last *screamer-results*)))
                   ;; Create a choice point and restore the
                   ;; probability value from the stored execution
                   ;; FIXME: Not sure why this wrapping `choice-point'
                   ;; is needed, but without it the probability
                   ;; doesn't get set properly
                   (choice-point
                    (factor-prob-nondeterministic
                     ;; Ignore the output of `factor-prob' and
                     ;; continue the continuation
                     (lambda (x)
                       (declare (ignore x))
                       (funcall continuation inp))
                     ;; Figure out what factor needs to be
                     ;; applied to reach `stored-prob' from
                     ;; the current probability
                     (/ stored-prob (current-probability))))))))
    (funcall f cont)))


(export '(collect-trail
          pure-values
          pure-one-value
          sample-optimizing
          normalize-probabilities
          factor-prob
          *possibility-consolidator*
          call/cc))



;;; FIXME: fix the below case to return (1 1 3 3)
(s:comment
  "If we add an x after the call to `cc-cache'
that x value gets returned, so looks like the
expansion isn't putting the call to the cc on
the direct path to the `all-values' collection
code?"
  (let (cc-cache)
    (all-values
      (let ((x (either 1 2 3 4)))
        (global
          ;; (print (list 'early cc-cache))
          (call/cc (lambda (cc)
                     (when (oddp x)
                       (setf cc-cache cc))
                     ;; (print (list 'within-cont cc-cache))
                     (funcall cc nil)))
          (print (list 'cc-cache cc-cache 'x x))
          (if (evenp x)
              (progn
                ;; (print (list 'x x))
                ;; (print (list 'internal cc-cache))
                (funcall cc-cache (1+ x)))
              (progn (print (list 'getting-x x)) x)))))))
