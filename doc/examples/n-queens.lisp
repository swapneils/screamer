(in-package :screamer-user)

;;; NOTE: Using iterate to make the
;;; loops simpler and more readable.
;;; This should work across standard-
;;; compliant platforms.
(use-package :iterate)

(cl:defun get-current-values (x)
  (etypecase x
    (null nil)
    (screamer::variable
     (screamer::variable-value x))
    (cons
     (cons (get-current-values (car x))
           (get-current-values (cdr x))))
    (t x)))

(defun attacks-diag (qi qj)
  "check if two queens attack diagonally"
  ;; Either order to simulate an `abs' form
  (orv
   (=v (+v (car qi) (cdr qi))
       (+v (car qj) (cdr qj)))
   (=v (-v (car qi) (cdr qi))
       (-v (car qj) (cdr qj)))))

(defun n-queens-internal (n)
  (let ((queens
          (local
            (iter (for i from 1 to n)
              ;; Coordinates
              (collect
                  ;; NOTE: Setting the first (row)
                  ;; coordinate to `i' since we
                  ;; know there must be one queen
                  ;; per row.
                  (cons i (an-integer-betweenv 1 n)))))))
    ;; NOTE: Unnecessary since we specify the row
    ;; manually
    ;; All rows are different
    ;; (assert! (all-differentv (mapcar #'car queens)))
    ;; All columns are different
    (assert! (all-differentv (mapcar #'cdr queens)))
    (local
      (iter (for i below (length queens))
        (iter (for j from (1+ i) below (length queens))
          ;; No queen attacks the following queens
          ;; diagonally
          (assert! (notv (attacks-diag (nth i queens) (nth j queens)))))))
    (solution queens (reorder #'domain-size (constantly nil) #'> #'linear-force))))

(defun n-queens (n &key (mode :count))
  "Find how many ways you can arrange N queens on an NxN chessboard
without any of them attacking each other."
  (ecase mode
    (:count (length (all-values (n-queens-internal n))))
    (:one (one-value (n-queens-internal n) 'failed))))
