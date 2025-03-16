(in-package :screamer-user)
(define-screamer-package :screamer-parallel
  (:use :alexandria :iterate)
  (:export #:p-a-member-of #:p-either
           #:p-an-element-of
           #:p-an-integer-between
           #:p-an-integer-above #:p-an-integer-below))
