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
            (funcall cc-internal nil))))
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
    (mapcar (lambda (cc) (funcall cc nil)) cc-list)
    (print *screamer-results*)))"
  (declare (ignorable f))
  (screamer-error
   "CALL/CC is a nondeterministic function. As such, it must be called only~%~
   from a nondeterministic context."))
(cl:defun call/cc-nondeterministic (continuation f)
  (declare (function continuation))
  ;; NOTE: Not declaring F a function in case it's something else
  ;; that's funcallable
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'p-a-member-of))

(cl:defun p-a-member-of (sequence)
  "EXPERIMENTAL
Parallel version of a-member-of.

The consequences of variables defined before this
function's invocation being mutated after said
invocation are undefined.

Does not exit until all nondeterministic paths have
succeeded, even when within a form that does not ordinarily
require completing all nondeterministic paths to return.

NOTE: Has not been tested with screamer::defun"
  (declare (ignore sequence))
  (screamer-error
   "P-A-MEMBER-OF is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

;;; FIXME: Doesn't work with one-value or n-values!
(cl:defun p-a-member-of-nondeterministic (continuation sequence)
  (serapeum:nest
   (let* ((sequence (value-of sequence)))
     (unless (serapeum:sequencep sequence)
       (fail)))
   (if (emptyp sequence) (fail))
   ;; Make a form to store the generated futures
   ;; FIXME: Currently futures keep running even after
   ;; leaving the nondeterministic form, unless they fail,
   ;; otherwise throw a signal, or finish.
   (let* (futures
          (q (lparallel.queue:make-queue))
          (escapes (lparallel.queue:make-queue))
          (accumulation-strategy (gethash :screamer-accumulation-strategy *nondeterministic-context* nil))
          ;; Get the actual type of accumulator
          (accumulation-type (typecase accumulation-strategy
                               (list (first accumulation-strategy))
                               (t accumulation-strategy)))
          (max-results (and (member accumulation-type '(:n-values))
                            (second accumulation-strategy)))
          ;; Store context lexically to copy it into the lparallel futures
          (context (copy-hash-table *nondeterministic-context*)))
     (declare (dynamic-extent futures q))
     (iter:iter (iter:for el in-sequence sequence)
       ;; Collect every promise into `futures'
       (push
        ;; Make a closure so we don't accidentally
        ;; reference the outer value of `el'
        (let ((el el))
          (lparallel:future
            (let* (
                   ;; Copy `*pure-cache*' in case the implementation
                   ;; doesn't use thread-safe hash-tables
                   ;; FIXME: This currently causes memory faults
                   ;; in SBCL, so pure-cache isn't copied
                   ;; into threads
                   (*pure-cache*
                     (when *pure-cache*
                       nil
                       ;; (copy-tree *pure-cache*)
                       ))
                   ;; Copy trail to avoid mangling between threads
                   (*trail* (copy-array *trail*))
                   ;; Copy value-collection objects
                   (*screamer-results* nil)
                   (*last-value-cons* nil)
                   (escaped t)
                   (*nondeterministic-context* (serapeum:dict)))
              ;; Copy over the nondeterministic context
              (when context
                (maphash (lambda (k v)
                           (typecase v
                             (list (setf (gethash k *nondeterministic-context*) (copy-tree v)))
                             (t (setf (gethash k *nondeterministic-context*) v))))
                         context))

              (catch '%escape
                (choice-point (funcall continuation el))
                (setf escaped nil))
              ;; Mark the escape
              (when escaped (lparallel.queue:push-queue 1 escapes))

              (dolist (result *screamer-results*)
                (lparallel.queue:push-queue result q)))))
        futures))
     ;; FIXME: Currently forcing futures to avoid the environment being deinstantiated,
     ;; causing errors in the future threads
     (mapcar #'lparallel:force futures)
     (iter:iter
       (iter:until
        (and (every #'lparallel:fulfilledp futures)
             (lparallel.queue:queue-empty-p q)))
       ;; Wait for queue to not be empty, then get values from it
       ;; NOTE: While this means the main thread is constantly
       ;; running instead of idling, it also means we don't need
       ;; to worry about cases where the queue doesn't get
       ;; populated but all futures exit
       (when (not (lparallel.queue:queue-empty-p q))
         (appendf *screamer-results* (list (lparallel.queue:pop-queue q)))
         ;; When max result count is reached, discard extra values and decide to escape
         (when (and max-results (> (length *screamer-results*) max-results))
           (setf *screamer-results* (subseq *screamer-results* 0 max-results))
           (lparallel.queue:push-queue 1 escapes))
         ;; Propagate escapes
         (unless (lparallel.queue:queue-empty-p escapes)
           ;; NOTE: We return the result list since some forms
           ;; use `%escape' to return results and others
           ;; ignore the returned value.
           (throw '%escape *screamer-results*)))
       (setf *last-value-cons* (last *screamer-results*)))
     ;; After all choice points are attempted, fail
     (fail))))

(cl:defmacro p-either (&rest options)
  `(funcall-nondeterministic (p-a-member-of (list ,@(iter:iter (iter:for opt in options) (iter:collect `(lambda () ,opt)))))))


(export '(collect-trail
          bounded?
          grounded?
          mapcar-nondeterministic
          pure-values
          pure-one-value
          sample
          sample-once
          sample-optimizing
          normalize-probabilities
          factor-prob
          *possibility-consolidator*
          *screamer-max-failures*
          call/cc
          p-a-member-of
          p-either))



;;; FIXME: fix the below case to return (1 1 3 3)
(s:comment
  "If we add an x after the call to `cc-cache'
that x value gets returned, so looks like the
expansion isn't putting the call to the cc on
the direct path to the `all-values' collection
code?"
  "I think the issue is in part that the
funcall's output (nil) is being tracked as well.
Still not sure why the continuation itself doesn't
push a value into `*screamer-results*', though; the
output series without the `push'es to `result-cache'
should be (1 1 nil 3 3 nil) given this model not
just (1 nil 3 nil)"
  (let (cc-cache
        result-cache)
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
          (push (print (list 'result
                             (if (evenp x)
                                 (progn
                                   ;; (print (list 'x x))
                                   ;; (print (list 'internal cc-cache))
                                   (funcall cc-cache (1+ x)))
                                 (progn (print (list 'getting-x x)) x))))
                result-cache))))))
