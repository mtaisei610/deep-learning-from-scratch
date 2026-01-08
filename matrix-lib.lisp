;; (declaim (optimize (speed 3) (safety 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3)))


(ql:quickload :cffi)

(defpackage :matrix
  (:use :cl :cffi)
  (:export :matrix
           :zeros
           :from-vector
           :copy
           :scalar
           :dot
           :show))

(in-package :matrix)


;; =======================================================
;;  Library Calling (OpenBLAS)
;; =======================================================

(define-foreign-library libopenblas
      (:darwin (:or "libopenblas.dylib" "libblas.dylib"))
      (:unix (:or "libopenblas.so" "libblas.so"))
      (:windows "libopenblas.dll")
      (t (:default "libopenblas")))

(use-foreign-library libopenblas)


;; X = \alpha * X
(defcfun ("cblas_sscal" %cblas-sscal) :void
  (n :int)
  (alpha :float)
  (x :pointer)
  (incx :int))

(defconstant +CblasRowMajor+ 101)
(defconstant +CblasColMajor+ 102)
(defconstant +CblasNoTrans+ 111)
(defconstant +CblasTrans+ 112)

;; C = \alpha*(A dot B) + \beta*C
;; m: row(A, C)
;; n: col(B, C)
;; k: col(A),row(B)
;; lda, ldb, ldc -> loading dimention (basically row(A))
(defcfun ("cblas_sgemm" %cblas-sgemm) :void
    (order :int)
    (transa :int)
    (transb :int)
    (m :int)
    (n :int)
    (k :int)
    (alpha :float)
    (a :pointer)
    (lda :int)
    (b :pointer)
    (ldb :int)
    (beta :float)
    (c :pointer)
    (ldc :int))

(defmacro nest-with-pointer-to-vector-data (bindings &body body)
  (reduce (lambda (binding inner)
            `(with-pointer-to-vector-data ,binding ,inner))
          bindings
          :from-end t
          :initial-value `(progn ,@body)))

(defun sgemm (m n k a b c alpha beta)
  (declare (type (simple-array single-float (*)) a b c)
           (type integer m n k)
           (type single-float alpha beta))
  (declare (optimize (debug 3)))
  (nest-with-pointer-to-vector-data
      ((ptr-a a)
       (ptr-b b)
       (ptr-c c))
    (%cblas-sgemm
     +CblasRowMajor+
     +CblasNoTrans+  ;; TransA
     +CblasNoTrans+  ;; TransB
     m n k
     alpha
     ptr-a
     k      ;; lda
     ptr-b
     n      ;; ldb
     beta
     ptr-c
     n))
  c)   ;; ldc



;; =======================================================
;;  Wrappers
;; =======================================================



(defstruct matrix
  "matrix data structure"
  (rows 0 :type fixnum)
  (cols 0 :type fixnum)
  (data #() :type (simple-array single-float (*))))

(defun zeros (rows cols)
  "create zero-filled matrix"
  (make-matrix :rows rows
	       :cols cols
	       :data (make-array (* rows cols)
				 :element-type 'single-float
				 :initial-element 0.0)))

(defun from-vector (rows cols vec)
  (declare (type fixnum rows)
           (type fixnum cols)
           (type (simple-array single-float (*))))
  "create matrix struct from vector"
  (assert (= (length vec) (* rows cols))
	  () "[from-vector] size unmatched.")
  (make-matrix :rows rows
	       :cols cols
	       :data (make-array (* rows cols)
				 :element-type 'single-float
				 :initial-contents vec)))

(defun show (mat)
  (declare (type matrix mat))
  "print matrix"
  (let ((m (min (matrix-rows mat) 10))
        (n (min (matrix-cols mat) 10))
        (d (matrix-data mat)))
    (format t "~%<MATRIX(~Ax~A)>~%" m n)
    (dotimes (i m)
      (dotimes (j n)
        (format t "~3F " (aref d (* i j))))
      (format t "~%"))))


(defun copy (mat)
  (declare (type matrix mat))
  "deep copy matrix"
  (let* ((len (* (matrix-rows mat) (matrix-cols mat)))
          (new-data (make-array len :element-type 'single-float)))
    (replace new-data (matrix-data mat))
    (make-matrix :rows (matrix-rows mat)
                 :cols (matrix-cols mat)
                 :data new-data)))

(defun scale (scalar mat)
  (declare (type single-float scalar)
           (type matrix mat))
  "multiple by a scalar"
  (let ((result (copy-matrix mat))
        (n (* (matrix-rows mat) (matrix-cols mat))))
    (with-pointer-to-vector-data (ptr (matrix-data result))
      (%cblas-sscal n (float scalar) ptr 1))
    result))


(defun dot (mat-a mat-b &optional (mat-c nil mat-c-p))
  (declare (type matrix mat-a)
           (type matrix mat-b))
  "matrix product: αA・B + C"
  (let ((m (matrix-rows mat-a))
	(k (matrix-cols mat-a))
	(k2 (matrix-rows mat-b))
	(n (matrix-cols mat-b)))
    (assert (= k k2) () "[matlib::dot] Dimention mismatch. (A,B)")
    (if mat-c-p
        (assert (= m (matrix-rows mat-c)) () "[matlib::dot] Dimention mismatch. (A,C)")
        (assert (= n (matrix-cols mat-c)) () "[matlib::dot] Dimention mismatch. (B,C)"))
    (let* ((ret-and-c (if mat-c-p (copy mat-c) (zeros m n)))
	   (arr-a (matrix-data mat-a))
	   (arr-b (matrix-data mat-b))
	   (arr-c (matrix-data mat-c)))
      (sgemm m n k arr-a arr-b arr-c 1.0 0.0)
      ret-and-c)))


;; Helper
(defun tof (&rest rest)
  "int args to vector(single-float)."
  (map 'vector (lambda (x) (coerce x 'single-float)) rest))

(let ((a (from-vector 2 3 (tof 1 2 3 4 5 6)))
      (b (from-vector 3 2 (tof 1 1 1 1 1 1)))
      (c (from-vector 2 2 (tof 10 10 10 10))))
  (show (dot a b c)))