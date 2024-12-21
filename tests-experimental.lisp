(in-package :screamer-tests)

(in-suite screamer-tests)

(deftest test-p-a-member-of ()
  (lparallel.kernel-util:with-temp-kernel ((serapeum:count-cpus))
    (is (equal (iota 2000)
               (sort (all-values (p-a-member-of (iota 2000))) #'<)))))
