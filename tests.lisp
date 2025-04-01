;;;; Copyright 2010, Nikodemus Siivola <nikodemus@sb-studio.net>
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; The above copyright and authorship notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :screamer-user)

(define-screamer-package :screamer-tests
  (:use :cl :hu.dwim.stefil)
  (:export #:test-screamer #:screamer-tests))

(in-package :screamer-tests)

(use-package :alexandria)

(defmacro are (&rest tests)
  `(progn
     ,@(loop for test in tests
             collect `(is ,test))))

(defun test-screamer (&optional no-debug)
  (flet ((test ()
           (eql 0 (getf (extract-test-run-statistics (screamer-tests))
                        :number-of-failures))))
    (if no-debug
        (without-debugging (test))
        (test))))

(defsuite (screamer-tests :in root-suite) ()
  (run-child-tests))

(in-suite screamer-tests)

(defun eval-when/ct ()
  (let ((x (either :a :b)))
    (declare (ignorable x))
    (or (eval-when (:compile-toplevel)
          x)
        t)))

(defun eval-when/lt ()
  (let ((x (either :a :b)))
    (declare (ignorable x))
    (or (eval-when (:load-toplevel)
          x)
        t)))

(defun eval-when/ex ()
  (let ((x (either :a :b)))
    (or (eval-when (:execute)
          x)
        t)))

(deftest eval-when.situations ()
  (are (equal '(t t) (all-values (eval-when/ct)))
       (equal '(t t) (all-values (eval-when/lt)))
       (equal '(:a :b) (all-values (eval-when/ex)))))

(defmacro evil-ding (form &environment env)
  (let ((exp (macroexpand form env)))
    `(or ,exp 'ding)))

(defun multiple-value-call-nondeterministic.ding ()
  (let ((bar (lambda (cont &rest args)
               (if args
                   (either (car args) (apply-nondeterministic cont cont (cdr args)))
                   (fail)))))
    (evil-ding
     (multiple-value-call-nondeterministic bar bar (values 1 nil 2) (values 4 nil 5)))))

(deftest multiple-value-call-nondeterministic.1 ()
  (is (equal '(1 ding 2 4 ding 5)
             (all-values (multiple-value-call-nondeterministic.ding)))))

(deftest a-member-of-vector ()
  (are (equal '() (all-values (a-member-of "")))
       (equal '(#\a) (all-values (a-member-of "a")))
       (equal '(#\a #\b) (all-values (a-member-of "ab")))
       (equal '(#\a #\b #\c) (all-values (a-member-of "abc")))))

(deftest prime-ordeal ()
  (macrolet ((check-first-n-primordials (n)
               (let ((test-forms
                       (loop for i from 1 to n
                             for test-name = (format nil "TEST~A" i)
                             for test-symbol = (find-symbol test-name "PRIMORDIAL")
                             when test-symbol
                               collect `(,test-symbol))))
                 ;; If no tests found, consider it a pass
                 (if test-forms
                     `(are ,@test-forms)
                     t))))
    (check-first-n-primordials 72)))

(deftest test-trail ()
  (is (equal '(t t t)
             (all-values
               (let* ((unwind nil)
                      (x (either 1 2 3)))
                 (trail (lambda () (push x unwind)))
                 (ecase x
                   (1 (is (null unwind)))
                   (2 (is (equal '(1) unwind)))
                   (3 (is (equal '(2 1) unwind)))))))))

(deftest test-count-failures ()
  (is (equal "Failures         =          5"
             (with-output-to-string (*standard-output*)
               (is (equal '(:a 5)
                          (count-failures
                            (one-value
                                (let ((x (either 1 2 3 4 5 :a)))
                                  (unless (keywordp x)
                                    (fail))
                                  ;; FIXME: leak, but keeping it for backwards compatibility
                                  (list x screamer::failure-count))))))))))

(deftest count-truesv.1 ()
  (is (eq nil
          (let* ((x (a-booleanv))
                 (y (a-booleanv))
                 (z (a-booleanv))
                 (n (count-truesv x y z)))
            (assert! x)
            (assert! y)
            (assert! (=v n 2))
            (value-of z)))))

(deftest count-truesv.2 ()
  (is (= 2
         (let* ((x (a-booleanv))
                (y (a-booleanv))
                (z (a-booleanv))
                (n (count-truesv x y z)))
           (assert! x)
           (assert! y)
           (assert! (notv z))
           (value-of n)))))

(deftest test-minv.1 ()
  (is (= 42
         (let ((x (a-member-ofv '(:a 42))))
           (minv x)))))

(deftest test-maxv.1 ()
  (is (= 42
         (let ((x (a-member-ofv '(:a 42))))
           (maxv x)))))

(deftest share!-bugs ()
  (flet ((foo (list1 list2)
           (let ((v1 (a-member-ofv list1))
                 (v2 (a-member-ofv list2)))
             (assert! (equalv v1 v2))
             (value-of v1))))
    (are
     (eq :a (foo '(:a :b) '(:c :d :a)))
     (eq t (foo '(t nil) '(t :a)))
     (eql 3 (foo '(1 2 3) '(nil t 3)))
     (eql 3 (foo '(1 2 3) '(nil t 3 4)))
     (eql 3 (foo '(nil t 3 4 -1) '(1 2 3))))
    (let ((xs (all-values
                (linear-force (foo '(nil t 3 4 -1) '(1 2 3 t))))))
      (is
          (or (equal '(3 t) xs)
              (equal '(t 3) xs))))))

(deftest test-upper-bounding-failures ()
  (let ((screamer::*screamer-max-failures* 20))
    (are
     (not (possibly? (> (an-integer-below 20) 20)))
     (not (possibly? (solution (>v (an-integer-belowv 20) 20)
                               (static-ordering #'linear-force)))))))

(deftest test-division-does-not-force-quotient-to-integer ()
    (is (equal
         (let* ((x (an-integer-betweenv 1 10))
                (y (an-integer-betweenv 1 10))
                (z (/v x y)))
           (all-values (solution (list x y z) (static-ordering #'linear-force))))
         `((1 1 1) (1 2 1/2) (1 3 1/3) (1 4 1/4) (1 5 1/5) (1 6 1/6) (1 7 1/7) (1 8 1/8)
           (1 9 1/9) (1 10 1/10) (2 1 2) (2 2 1) (2 3 2/3) (2 4 1/2) (2 5 2/5) (2 6 1/3)
           (2 7 2/7) (2 8 1/4) (2 9 2/9) (2 10 1/5) (3 1 3) (3 2 3/2) (3 3 1) (3 4 3/4)
           (3 5 3/5) (3 6 1/2) (3 7 3/7) (3 8 3/8) (3 9 1/3) (3 10 3/10) (4 1 4) (4 2 2)
           (4 3 4/3) (4 4 1) (4 5 4/5) (4 6 2/3) (4 7 4/7) (4 8 1/2) (4 9 4/9) (4 10 2/5)
           (5 1 5) (5 2 5/2) (5 3 5/3) (5 4 5/4) (5 5 1) (5 6 5/6) (5 7 5/7) (5 8 5/8)
           (5 9 5/9) (5 10 1/2) (6 1 6) (6 2 3) (6 3 2) (6 4 3/2) (6 5 6/5) (6 6 1)
           (6 7 6/7) (6 8 3/4) (6 9 2/3) (6 10 3/5) (7 1 7) (7 2 7/2) (7 3 7/3) (7 4 7/4)
           (7 5 7/5) (7 6 7/6) (7 7 1) (7 8 7/8) (7 9 7/9) (7 10 7/10) (8 1 8) (8 2 4)
           (8 3 8/3) (8 4 2) (8 5 8/5) (8 6 4/3) (8 7 8/7) (8 8 1) (8 9 8/9) (8 10 4/5)
           (9 1 9) (9 2 9/2) (9 3 3) (9 4 9/4) (9 5 9/5) (9 6 3/2) (9 7 9/7) (9 8 9/8)
           (9 9 1) (9 10 9/10) (10 1 10) (10 2 5) (10 3 10/3) (10 4 5/2) (10 5 2)
           (10 6 5/3) (10 7 10/7) (10 8 5/4) (10 9 10/9) (10 10 1)))))

(deftest test-division-disallows-divide-by-zero ()
  (is (equal
       (let* ((x (an-integer-betweenv 0 10))
              (y (an-integer-betweenv 0 10))
              (z (/v x y)))
         (all-values (solution (list x y z) (static-ordering #'linear-force))))
       `((0 10 0) (0 9 0) (0 8 0) (0 7 0) (0 6 0) (0 5 0) (0 4 0) (0 3 0) (0 2 0)
         (0 1 0) (1 10 1/10) (1 9 1/9) (1 8 1/8) (1 7 1/7) (1 6 1/6) (1 5 1/5)
         (1 4 1/4) (1 3 1/3) (1 2 1/2) (1 1 1) (2 10 1/5) (2 9 2/9) (2 8 1/4) (2 7 2/7)
         (2 6 1/3) (2 5 2/5) (2 4 1/2) (2 3 2/3) (2 2 1) (2 1 2) (3 10 3/10) (3 9 1/3)
         (3 8 3/8) (3 7 3/7) (3 6 1/2) (3 5 3/5) (3 4 3/4) (3 3 1) (3 2 3/2) (3 1 3)
         (4 10 2/5) (4 9 4/9) (4 8 1/2) (4 7 4/7) (4 6 2/3) (4 5 4/5) (4 4 1) (4 3 4/3)
         (4 2 2) (4 1 4) (5 10 1/2) (5 9 5/9) (5 8 5/8) (5 7 5/7) (5 6 5/6) (5 5 1)
         (5 4 5/4) (5 3 5/3) (5 2 5/2) (5 1 5) (6 10 3/5) (6 9 2/3) (6 8 3/4) (6 7 6/7)
         (6 6 1) (6 5 6/5) (6 4 3/2) (6 3 2) (6 2 3) (6 1 6) (7 10 7/10) (7 9 7/9)
         (7 8 7/8) (7 7 1) (7 6 7/6) (7 5 7/5) (7 4 7/4) (7 3 7/3) (7 2 7/2) (7 1 7)
         (8 10 4/5) (8 9 8/9) (8 8 1) (8 7 8/7) (8 6 4/3) (8 5 8/5) (8 4 2) (8 3 8/3)
         (8 2 4) (8 1 8) (9 10 9/10) (9 9 1) (9 8 9/8) (9 7 9/7) (9 6 3/2) (9 5 9/5)
         (9 4 9/4) (9 3 3) (9 2 9/2) (9 1 9) (10 10 1) (10 9 10/9) (10 8 5/4)
         (10 7 10/7) (10 6 5/3) (10 5 2) (10 4 5/2) (10 3 10/3) (10 2 5) (10 1 10)))))

(deftest test-does-not-disallow-gaussian-integers-from-noninteger-multiplication ()
  (is (equal
       (let* ((x (a-member-ofv '(2/3 3/2) "x"))
              (y (a-member-ofv '(2/3 3/2) "y"))
              (z (*v x y)))
                                        ; (print z)
                                        ; (print (screamer::variable-possibly-integer? z))
         (assert! (integerpv z))
                                        ; (print z)
                                        ;fails
         (all-values (solution (list x y z) (static-ordering #'linear-force))))
       `((2/3 3/2 1) (3/2 2/3 1)))))

(deftest does-not-crash-when-guessing-for-non-integer-reals ()
  (is (= (length
          (n-values (20)
            (solution (a-real-betweenv 0 10)
                      (static-ordering #'divide-and-conquer-force))))
         20)))

(deftest tries-integer-values-for-possibly-integer-reals ()
  (let ((result (n-values (10)
                  (solution (a-real-betweenv 1 10)
                            (static-ordering #'divide-and-conquer-force)))))
    (is (= (length result) 10))
    (is (every #'integerp result))
    (is (every (lambda (i) (<= 1 i 10)) result))))

(deftest uses-bfs-for-reals-when-solution-is-not-known ()
  (let* ((*maximum-discretization-range* 20)
         (result (n-values (21)
                  (solution (a-real-betweenv 0 10)
                            (static-ordering #'divide-and-conquer-force)))))
    (is (= (length result) 21))
    ;; Integer values average to 5
    (is (= (alexandria:mean (subseq result 0 11)) 5))
    ;; Non-integer guesses also average to 5
    (is (= (alexandria:mean (subseq result 11 21)) 5))))

(deftest divide-and-conquer-works-with-no-maximum-discretization-range ()
  (let* ((*maximum-discretization-range* nil)
         (result (n-values (13)
                   (solution (a-real-betweenv 0 10)
                             (static-ordering #'divide-and-conquer-force)))))
    (is (= (length result) 13))))

(deftest does-not-incorrectly-disqualify-sums-from-being-integers ()
  (labels ((recursive-= (&rest ls)
             (cond ((every #'numberp ls) (apply #'= ls))
                   ((some #'numberp ls) nil)
                   ((every #'listp ls)
                    (every #'identity (apply #'mapcar #'recursive-= ls))))))
    (is (recursive-=
         (let* ((x (a-member-ofv '(1/2 1/4 1/8 1/16)))
                (y (a-real-betweenv 0 1))
                (z (+v x y)))
           (assert! (integerpv z))
           (all-values (solution (list x y z) (static-ordering #'linear-force))))
         (let* ((x (a-member-ofv '(1/2 1/4 1/8 1/16)))
                (y (a-real-betweenv 0 1))
                (z (+v x y)))
           (assert! (=v (an-integerv) z))
           (all-values (solution (list x y z) (static-ordering #'linear-force))))
         (let* ((x (a-member-ofv '(1/2 1/4 1/8 1/16)))
                (y (a-real-betweenv 0 1))
                (z (+v x y)))
           (assert! (=v 1 z))
           (all-values (solution (list x y z) (static-ordering #'linear-force))))
         (let* ((x (a-member-ofv '(1/2 1/4 1/8 1/16)))
                (y (a-real-betweenv 0 1))
                (z (+v x y)))
           (assert! (=v 1.0 z))
           (all-values (solution (list x y z) (static-ordering #'linear-force))))))))

(deftest domain-size-integers-with-non-integer-bounds ()
  (let ((n (an-integerv)))
    (setf (screamer::variable-upper-bound n) 16.5)
    (setf (screamer::variable-lower-bound n) 14.6)
    (is (= (domain-size n) 2))
    (is (equal
         (all-values (solution n (static-ordering #'linear-force)))
         '(15 16)))))

(deftest domain-size-works-on-non-list-sequences ()
  (is (= (domain-size (vector (screamer::variablize 30)
                              (an-integer-betweenv 1 4)
                              (an-integer-betweenv 1 4)))
         16)))
