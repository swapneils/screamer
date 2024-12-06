(in-package :screamer)

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

(let* (cc
       (f (lambda (cc-internal) (setf cc cc-internal))))
  (all-values
    (let ((x (either 1 2 3)))
      (print (call/cc f))
      (print 'hi)
      (print x)))
  (let (*screamer-results*)
    (choice-point (funcall cc 'hello))
    (print *screamer-results*)
    (choice-point (funcall cc 'hello))
    (print *screamer-results*)
))

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
  ;; (print "starting call/cc")
  (let ((cont (lambda (inp)
                (choice-point
                 (funcall continuation inp)))))
    (funcall f cont))
  ;; (fail)
  ;; (print "continuing call/cc")
  ;; (funcall continuation nil)
  ;; (print "leaving call/cc")
  )


(export '(collect-trail
          pure-values
          pure-one-value
          sample-optimizing
          normalize-probabilities
          factor-prob
          *possibility-consolidator*
          call/cc))
(let* (cc
       (f (lambda (cc-internal) (setf cc cc-internal))))
  (all-values-prob
    (print (call/cc f))
    (print 'hi)
    (print (either 1 2 3)))
  (let (*screamer-results*)
    (choice-point (funcall cc 'hello))
    (print *screamer-results*)))
