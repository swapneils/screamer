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
    (is (equal (append (iota 1000) (iota 2000))
               (all-values (p-an-integer-between 0 (1- (p-either 1000 2000))))))
    (is (equal (list :hi :hi :hi :hi)
               (all-values (p-a-member-of '(1 2)) (p-a-member-of '(3 4)) :hi)))))

(defun p-screamer-flatten (x)
  (let ((ret (p-a-member-of x)))
    (if (listp ret)
        (p-screamer-flatten ret)
        ret)))

(deftest test-parallel-screamer-defun ()
  (lparallel.kernel-util:with-temp-kernel ((serapeum:count-cpus))
    (let ((ret (all-values (p-screamer-flatten '(1 2 3 4 (4 5 6) '(7 8 9))))))
      (is (member 'quote ret))
      (is (equal '(1 2 3 4 4 5 6 7 8 9) (remove 'quote ret))))))
