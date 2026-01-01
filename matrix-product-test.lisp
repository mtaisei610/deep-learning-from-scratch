(ql:quickload :cffi :silent t)

(defpackage :blas-sample
  (:use :cl :cffi))
(in-package :blas-sample)

(define-foreign-library libblas
  (:unix (:or "libopenblas.so" "libopenblas.so.0" "libblas.so"))
  (t (:default "libblas")))

(use-foreign-library libblas)

(defconstant +CblasRowMajor+ 101)
(defconstant +CblasNoTrans+ 111)

(defcfun ("cblas_dgemm" cblas-dgemm) :void
  (order :int)
  (trans-a :int)
  (trans-b :int)
  (m :int)
  (n :int)
  (k :int)
  (alpha :double)
  (a :pointer)
  (lda :int)
  (b :pointer)
  (ldb :int)
  (beta :double)
  (c :pointer)
  (ldc :int))

(defun run-multiplication ()
  (let* ((m 2)
         (k 2)
         (n 2)
         (a-data #(1.0d0 6.0d0
                   3.0d0 8.0d0))
         (b-data #(5.0d0 6.0d0
                   7.0d0 8.0d0))
         (c-data (make-array 4 :element-type 'double-float :initial-element 0.0d0)))
    (format t "Matrix A: ~A~%" a-data)
    (format t "Matrix B: ~A~%" b-data)
    (with-foreign-objects ((ptr-a :double 4)
                           (ptr-b :double 4)
                           (ptr-c :double 4))
      (dotimes (i 4)
        (setf (mem-aref ptr-a :double i) (aref a-data i))
        (setf (mem-aref ptr-b :double i) (aref b-data i))
        (setf (mem-aref ptr-c :double i) 0.0d0))

      (cblas-dgemm +CblasRowMajor+
                    +CblasNoTrans+ +CblasNoTrans+
                    m n k
                    1.0d0
                    ptr-a m
                    ptr-b n
                    0.0d0
                    ptr-c n)

      (dotimes (i 4)
        (setf (aref c-data i) (mem-aref ptr-c :double i)))

      (format t "Result Matrix C: ~A~%" c-data))))

(run-multiplication)
