(in-package :screamer-parallel)
(import '(serapeum:count-cpus))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (screamer::declare-nondeterministic 'p-a-member-of))

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
Note that this method requires the implementation
to share lexical variables with new threads. This
is NOT guaranteed on all implementations.

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
  (screamer::screamer-error
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
          (max-failures screamer::*screamer-max-failures*))
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
             ;; FIXME: We can't assume that lexical variables
             ;; are carried into `bt:make-thread', the
             ;; underlying implementation behind `lparallel:future'
             ;; TODO: Make a global variable to store the context
             ;; information and set it manually every time we
             ;; create these futures? Within a single thread it
             ;; shouldn't be invoked more than once, and if
             ;; we use a semaphore to only run child threads once
             ;; the parent is ready, there won't be nested conflicts.
             (let ((el el))
               (lparallel:future
                 (let* (
                        ;; Copy `*pure-cache*' in case the implementation
                        ;; doesn't use thread-safe hash-tables
                        (screamer::*pure-cache*
                          (when screamer::*pure-cache* (copy-tree screamer::*pure-cache*)))
                        ;; Copy trail to avoid mangling between threads
                        (screamer::*trail* (copy-array screamer::*trail*))
                        ;; Copy value-collection objects
                        (screamer::*screamer-results* nil)
                        (screamer::*last-value-cons* nil)
                        (screamer::*screamer-max-failures* max-failures)
                        (escaped t)
                        (screamer::*nondeterministic-context* (serapeum:dict))
                        ;; Trackers for incrementally publishing
                        ;; collected `*screamer-results*'
                        accumulator-config-tracker
                        (last-screamer-results-size 0))
                   (declare (dynamic-extent escaped
                                            accumulator-config-tracker
                                            last-screamer-results-size)
                            (boolean escaped)
                            (vector screamer::*trail*)
                            (type (integer 0) last-screamer-results-size)
                            (list screamer::*screamer-results* screamer::*last-value-cons* screamer::*pure-cache*))
                   ;; Copy over the nondeterministic context
                   (when context
                     (maphash (lambda (k v)
                                (typecase v
                                  (list (setf (gethash k screamer::*nondeterministic-context*) (copy-tree v)))
                                  (t (setf (gethash k screamer::*nondeterministic-context*) v))))
                              context))
                   ;; Track the starting `:screamer-accumulation-strategy'
                   ;; so we know if we're inside a nested accumulator
                   (setf accumulator-config-tracker (gethash :screamer-accumulation-strategy screamer::*nondeterministic-context*))

                   (when-failing ((unless (lparallel.queue:queue-empty-p escapes)
                                    ;; Return the results with the escape in case
                                    ;; some inner form needs them
                                    (screamer::escape screamer::*screamer-results*))

                                   ;; Only publish incrementally when unordered and
                                   ;; in the same Screamer accumulation context as
                                   ;; this thread started with
                                   (when (and (not ordered)
                                              (eql accumulator-config-tracker
                                                   (gethash :screamer-accumulation-strategy screamer::*nondeterministic-context*)))
                                     ;; When new values have been added
                                     (when (> (length screamer::*screamer-results*) last-screamer-results-size)
                                       ;; Push the new values to the queue
                                       (dolist (result (subseq screamer::*screamer-results* last-screamer-results-size))
                                         (lparallel.queue:push-queue result q))
                                       ;; Update the result-count tracker
                                       (setf last-screamer-results-size (length screamer::*screamer-results*)))))
                     (catch 'screamer::%escape
                       (screamer::choice-point (funcall continuation el))
                       (setf escaped nil)))
                   ;; Mark the escape
                   (when (and escaped
                              ;; NOTE: Not counting escapes from `n-values'
                              ;; forms if `max-results' was reached, since
                              ;; they use `%escape' to exit once they have
                              ;; enough results.
                              (or (not max-results)
                                  (> (length screamer::*screamer-results*) max-results)))
                     (lparallel.queue:push-queue 1 escapes))
                   ;; (print (list "escaped-thread" screamer::*screamer-results*))

                   ;; Add to the unordered-result queue any values that weren't
                   ;; added in `when-failing' for whatever reason
                   (unless ordered
                     (dolist (result (subseq screamer::*screamer-results* last-screamer-results-size))
                       (lparallel.queue:push-queue result q)))

                   ;; Return screamer-results for when we're returning answers in order
                   screamer::*screamer-results*)))
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
                   (screamer::escape screamer::*screamer-results*)))
               (check-max-results ()
                 (when (and max-results (> (length screamer::*screamer-results*) max-results))
                   (serapeum:callf #'subseq screamer::*screamer-results* 0 max-results)
                   (lparallel.queue:push-queue 1 escapes))))
            (declare (inline maybe-propagate-escapes check-max-results))
            (cond
              ;; Getting results in order
              (ordered
               (iter:iter
                 (iter:for f in futures)

                 ;; Force the future and collect its results
                 (iter:for result = (lparallel:force f))
                 (appendf screamer::*screamer-results* result)

                 ;; When max result count is reached, discard extra values and decide to escape
                 (check-max-results)

                 ;; Update `*last-value-cons*' for bookkeeping reasons
                 (setf screamer::*last-value-cons* (last screamer::*screamer-results*))

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
                   (appendf screamer::*screamer-results* (list (lparallel.queue:pop-queue q)))

                   ;; When max result count is reached, discard extra values and decide to escape
                   (check-max-results)

                   ;; Update `screamer::*last-value-cons*' for bookkeeping reasons
                   (setf screamer::*last-value-cons* (last screamer::*screamer-results*)))

                 ;; Propagate escapes
                 (maybe-propagate-escapes)))))

          ;; After all choice points are attempted, exit
          (cond
            ;; If in an `n-values' form and reaching the required result-count, call `escape' with the results
            ((and max-results
                  (= (length screamer::*screamer-results*) max-results))
             (screamer::escape screamer::*screamer-results*))
            ;; Otherwise just `fail'
            (t (fail))))
     ;; Check again to ensure all futures have been forced before exiting, in case
     ;; we have an unplanned exit
     (mapcar #'lparallel:force futures))))

(defun p-an-element-of (collection)
  "Parallel version of AN-ELEMENT-OF."
  (p-a-member-of (screamer::collection-to-sequence collection)))

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
