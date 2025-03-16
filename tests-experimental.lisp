(in-package :screamer-user)

(define-screamer-package :screamer-tests
  (:use :cl :hu.dwim.stefil :screamer-parallel)
  (:export #:test-screamer #:screamer-tests))

(in-package :screamer-tests)

(in-suite screamer-tests)

(deftest test-p-a-member-of ()
  (lparallel.kernel-util:with-temp-kernel ((serapeum:count-cpus))
    (is (equal (iota 2000) (all-values (p-a-member-of (iota 2000)))))))

(deftest test-p-an-integer-between ()
  (lparallel.kernel-util:with-temp-kernel ((serapeum:count-cpus))
    (is (equal (iota 2000) (all-values (p-an-integer-between 0 (1- 2000)))))))

(deftest test-nested-parallelism ()
  (lparallel.kernel-util:with-temp-kernel ((serapeum:count-cpus))
    (are
     (equal (sort (append (iota 1000) (iota 2000)) #'<)
            (sort (all-values (p-an-integer-between 0 (1- (p-either 1000 2000)))) #'<))
     (equal (list :hi :hi :hi :hi)
            (all-values (p-a-member-of '(1 2)) (p-a-member-of '(3 4)) :hi)))))

(defun p-screamer-flatten (x)
  (let ((ret (p-a-member-of x)))
    (if (listp ret)
        (p-screamer-flatten ret)
        ret)))

(deftest test-parallel-screamer-defun ()
  (lparallel.kernel-util:with-temp-kernel ((serapeum:count-cpus))
    (let ((ret (all-values (p-screamer-flatten '(1 2 3 4 (4 5 6) '(7 8 9))))))
      (are
       (member 'quote ret)
       (equal '(1 2 3 4 4 5 6 7 8 9) (remove 'quote ret))))))
