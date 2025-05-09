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

;;; TODO: Add a `when-succeeding' form that allows you
;;; to take additional actions when a collection form
;;; is succeeding. This would allow collecting intermediate
;;; results from searches, which would be more efficient
;;; in the `ORDERED'=`NIL' case since you don't need
;;; to wait for every thread to reach `max-results'
;;; or run out of possibilities before accumulating its
;;; values.
;;; NOTE: This also needs some way to not affect
;;; any nested Screamer accumulation forms within
;;; the one you want to use `when-succeeding' for.
(cl:defun p-a-member-of (sequence &key (ordered t))
  "EXPERIMENTAL
Parallel version of a-member-of.

All forms which would be evaluated after this one
are instead evaluated in multiple different threads,
one for each member of SEQUENCE. The results from
these forms are collected into `*screamer-results*'
as per the behavior of the closest parent Screamer
aggregation form (i.e. `all-values', `n-values', etc.).

The consequences of variables defined before this
function's invocation being mutated after said
invocation are undefined.

When ORDERED is T, results are only collected from threads
in the order of their corresponding members of SEQUENCE.
When it is NIL, each thread's results will be collected
on completion regardless of where its corresponding value
is in SEQUENCE.

When ORDERED is T, a thread's results are only collected once
the thread itself has completed. When it is NIL, results
are collected incrementally from each thread as they are
collected.
Note that if `p-a-member-of' is used within forms such as `one-value'
or `n-values', their behavior of only requiring a certain number
of results is carried over into the child threads, though
each thread individually needs to reach that max result
count to exit before all nondeterministic generators have been
exhausted.

WARNING: Lexical environments are shared between parallel threads.
WARNING: Dynamic environment variables may or may not be nullified within
a parallel thread.
This is due to the underlying threading implementation (`lparallel'); please
defer to `lparallel' and `bordeaux-threads' for the behavior of thread
variables."
  (declare (ignore sequence ordered))
  (screamer-error
   "P-A-MEMBER-OF is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun p-a-member-of-nondeterministic (continuation sequence
                                          &key (ordered t)
                                          &aux (sequence (value-of sequence)))
  (declare (optimize (speed 3) (space 3))
           (function continuation))
  (serapeum:nest
   ;; NOTE: Moved to the &aux definition
   ;; (let* ((sequence (value-of sequence))))
   (serapeum:if-not (serapeum:sequencep sequence) (fail))
   ;; Fail immediately if there are no values to consider
   ;; NOTE: This should be done automatically by correct operation of the below...
   (if (emptyp sequence) (fail))
   ;; Make a form to store the generated futures
   ;; FIXME: Currently futures keep running even after
   ;; leaving the nondeterministic form, unless they fail,
   ;; otherwise throw a signal, or finish.
   ;; NOTE: Mitigated by forcing all futures before exit
   ;; and making them exit early if the main thread
   ;; is tracking any `escape' calls.
   (let* (futures
          (q (lparallel.queue:make-queue))
          (escapes (lparallel.queue:make-queue))
          (accumulation-strategy (gethash :screamer-accumulation-strategy *nondeterministic-context* nil))
          ;; Get the actual type of accumulator
          (accumulation-type (first accumulation-strategy))
          (max-results (and (member accumulation-type '(:n-values))
                            (second accumulation-strategy)))
          ;; Store context lexically to copy it into the lparallel futures
          (context (copy-hash-table *nondeterministic-context*))
          ;; Bind max failures dynamically
          ;; TODO: Find a way to change *screamer-max-failures*
          ;; within futures? If we could do this it would
          ;; let us short-circuit infinite searches
          ;; once the top-level has succeeded
          ;; (*screamer-max-failures* *screamer-max-failures*)
          ;; Bind max failures lexically to copy into future
          (max-failures *screamer-max-failures*))
     (declare (dynamic-extent futures context)
              (dynamic-extent q escapes)
              (type (or null (integer 0)) max-results)
              (hash-table context)))
   ;; Unwind-protect to ensure futures are cleaned up even for
   ;; unexpected exits
   (unwind-protect
        (progn
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
                        (*pure-cache*
                          (when *pure-cache* (copy-tree *pure-cache*)))
                        ;; Copy trail to avoid mangling between threads
                        (*trail* (copy-array *trail*))
                        ;; Copy value-collection objects
                        (*screamer-results* nil)
                        (*last-value-cons* nil)
                        (*screamer-max-failures* max-failures)
                        (escaped t)
                        (*nondeterministic-context* (serapeum:dict))
                        ;; Trackers for incrementally publishing
                        ;; collected `*screamer-results*'
                        accumulator-config-tracker
                        (last-screamer-results-size 0))
                   (declare (dynamic-extent escaped
                                            accumulator-config-tracker
                                            last-screamer-results-size)
                            (boolean escaped)
                            (vector *trail*)
                            (type (integer 0) last-screamer-results-size)
                            (list *screamer-results* *last-value-cons* *pure-cache*))
                   ;; Copy over the nondeterministic context
                   (when context
                     (maphash (lambda (k v)
                                (typecase v
                                  (list (setf (gethash k *nondeterministic-context*) (copy-tree v)))
                                  (t (setf (gethash k *nondeterministic-context*) v))))
                              context))
                   ;; Track the starting `:screamer-accumulation-strategy'
                   ;; so we know if we're inside a nested accumulator
                   (setf accumulator-config-tracker (gethash :screamer-accumulation-strategy *nondeterministic-context*))

                   (when-failing ((unless (lparallel.queue:queue-empty-p escapes)
                                    ;; Return the results with the escape in case
                                    ;; some inner form needs them
                                    (escape *screamer-results*))

                                   ;; Only publish incrementally when unordered and
                                   ;; in the same Screamer accumulation context as
                                   ;; this thread started with
                                   (when (and (not ordered)
                                              (eql accumulator-config-tracker
                                                   (gethash :screamer-accumulation-strategy *nondeterministic-context*)))
                                     ;; When new values have been added
                                     (when (> (length *screamer-results*) last-screamer-results-size)
                                       ;; Push the new values to the queue
                                       (dolist (result (subseq *screamer-results* last-screamer-results-size))
                                         (lparallel.queue:push-queue result q))
                                       ;; Update the result-count tracker
                                       (setf last-screamer-results-size (length *screamer-results*)))))
                     (catch '%escape
                       (choice-point (funcall continuation el))
                       (setf escaped nil)))
                   ;; Mark the escape
                   (when (and escaped
                              ;; NOTE: Not counting escapes from `n-values'
                              ;; forms if `max-results' was reached, since
                              ;; they use `%escape' to exit once they have
                              ;; enough results.
                              (or (not max-results)
                                  (> (length *screamer-results*) max-results)))
                     (lparallel.queue:push-queue 1 escapes))
                   ;; (print (list "escaped-thread" *screamer-results*))

                   ;; Add to the unordered-result queue any values that weren't
                   ;; added in `when-failing' for whatever reason
                   (unless ordered
                     (dolist (result (subseq *screamer-results* last-screamer-results-size))
                       (lparallel.queue:push-queue result q)))

                   ;; Return screamer-results for when we're returning answers in order
                   *screamer-results*)))
             futures))
          ;; Sort futures since they were pushed in reverse order
          (serapeum:callf #'nreverse futures)

          (flet
              ((maybe-propagate-escapes ()
                 (unless (lparallel.queue:queue-empty-p escapes)
                   ;; Force all futures to avoid the context being dereferenced while
                   ;; the future still needs it
                   ;; NOTE: The futures are configured to escape on their first fail
                   ;; if `escapes' is non-empty, so it shouldn't take long to force
                   ;; them all.
                   (mapcar #'lparallel:force futures)
                   ;; NOTE: We return the result list, since some forms
                   ;; use `%escape' to return results while others
                   ;; ignore the returned value.
                   (escape *screamer-results*)))
               (check-max-results ()
                 (when (and max-results (> (length *screamer-results*) max-results))
                   (serapeum:callf #'subseq *screamer-results* 0 max-results)
                   (lparallel.queue:push-queue 1 escapes))))
            (declare (inline maybe-propagate-escapes check-max-results))
            (cond
              ;; Getting results in order
              (ordered
               (iter:iter
                 (iter:for f in futures)

                 ;; Force the future and collect its results
                 (iter:for result = (lparallel:force f))
                 (appendf *screamer-results* result)

                 ;; When max result count is reached, discard extra values and decide to escape
                 (check-max-results)

                 ;; Update `*last-value-cons*' for bookkeeping reasons
                 (setf *last-value-cons* (last *screamer-results*))

                 ;; Propagate escapes
                 (maybe-propagate-escapes)))

              ;; Getting results out of order
              ((not ordered)
               (iter:iter
                 (iter:until
                  (and (every #'lparallel:fulfilledp futures)
                       (lparallel.queue:queue-empty-p q)))

                 ;; Wait for queue to not be empty, then get values from it
                 (when (not (lparallel.queue:queue-empty-p q))
                   ;; Collect the queue's top value into `*screamer-results*'
                   (appendf *screamer-results* (list (lparallel.queue:pop-queue q)))

                   ;; When max result count is reached, discard extra values and decide to escape
                   (check-max-results)

                   ;; Update `*last-value-cons*' for bookkeeping reasons
                   (setf *last-value-cons* (last *screamer-results*)))

                 ;; Propagate escapes
                 (maybe-propagate-escapes)))))

          ;; After all choice points are attempted, exit
          (cond
            ;; If in an `n-values' form and reaching the required result-count, call `escape' with the results
            ((and max-results
                  (= (length *screamer-results*) max-results))
             (escape *screamer-results*))
            ;; Otherwise just `fail'
            (t (fail))))
     ;; Check again to ensure all futures have been forced before exiting, in case
     ;; we have an unplanned exit
     (mapcar #'lparallel:force futures))))

(defun p-an-element-of (collection)
  "Parallel version of AN-ELEMENT-OF."
  (p-a-member-of (collection-to-sequence collection)))

(cl:defmacro p-either (&rest options)
  "Like EITHER, but runs all options in parallel.
Does NOT return the results from each member of
OPTIONS in order.
Behaves equivalently to `p-a-member-of' with
ORDERED=NIL and each element of options evaluated
within its corresponding thread."
  `(funcall-nondeterministic
    (p-a-member-of (list ,@(iter:iter (iter:for opt in options) (iter:collect `(lambda () ,opt))))
                   :ordered nil)))

(defun p-an-integer-between (low high)
  (let ((low (ceiling (value-of low)))
        (high (floor (value-of high))))
    (if (> low high) (fail)
        (p-a-member-of (alexandria:iota (1+ (- high low)) :start low)))))

(defun p-an-integer-above (low)
  "Note: Using `*maximum-discretization-range*' to decide
how many threads to run at once"
  (let* ((low (value-of low))
         (high (+ low *maximum-discretization-range*)))
    (declare (dynamic-extent high))
    (either
      (p-a-member-of (alexandria:iota *maximum-discretization-range* :start low))
      (p-an-integer-above high))))
(defun p-an-integer-below (high)
  "Note: Using `*maximum-discretization-range*' to decide
how many threads to run at once"
  (let* ((high (value-of high))
         (low (- high *maximum-discretization-range*)))
    (declare (dynamic-extent high))
    (either
      (p-a-member-of (nreverse (alexandria:iota *maximum-discretization-range* :start (1+ low))))
      (p-an-integer-below low))))

(defmacro-compile-time lambda-nondeterministic (args &body body)
  "Defines an unnamed Screamer function-object, e.g. to reuse
between multiple Screamer forms.

Note that these MUST be called via `funcall-nondeterministic',
and are incompatible with the standard `funcall'."
  `(one-value (lambda ,args ,@body)))

(export '(collect-trail
          uniquely?
          bounded?
          grounded?
          mapcar-nondeterministic
          == /==
          pure-values
          pure-one-value
          an-element-of
          sample
          sample-once
          sample-optimizing
          normalize-probabilities
          factor-prob
          *possibility-consolidator*
          *screamer-max-failures*
          lambda-nondeterministic
          call/cc))



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
