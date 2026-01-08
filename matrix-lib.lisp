;; (declaim (optimize (speed 3) (safety 0)))
(declaim (optimize (speed 0) (safety 3) (debug 3)))


(ql:quickload :cffi)


(defpackage :matrix
  (:use :cl :cffi)
  (:export :matrix
           :make-matrix
           :free-matrix
           :mref
           :print-matrix
           :sgemm!
           :saxpy!
           :sscal!
           :clone-matrix
           :copy-matrix!
           :fill-random!
           :matrix-map!
           :hadamard!
           :add-vector!
           :sum-all
           :argmax))

(in-package :matrix)


;; =======================================================
;;  Load OpenBLAS
;; =======================================================

(define-foreign-library libopenblas
      (:darwin (:or "libopenblas.dylib" "libblas.dylib"))
      (:unix (:or "libopenblas.so" "libblas.so"))
      (:windows "libopenblas.dll")
      (t (:default "libopenblas")))

(use-foreign-library libopenblas)


;; =======================================================
;;  Define C Functions (CFFI)
;; =======================================================

;; === Constants ===
(defconstant +CblasRowMajor+ 101)
(defconstant +CblasColMajor+ 102)
(defconstant +CblasNoTrans+ 111)
(defconstant +CblasTrans+ 112)

;; === Matrix Dot ===
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

;; === Vector Dot ===
(defcfun ("cblas_saxpy" %cblas-saxpy) :void
  (n :int)
  (alpha :float)
  (x :pointer)
  (incx :int)
  (y :pointer)
  (incy :int))

;; === Scalar Dot ===
(defcfun ("cblas_sscal" %cblas-sscal) :void
  (n :int)
  (alpha :float)
  (x :pointer)
  (incx :int))

;; === Copy Matrix ===
(defcfun ("cblas_scopy" %cblas-scopy) :void
  (n :int)
  (x :pointer) (incx :int)
  (y :pointer) (incy :int))


;; =======================================================
;;  Matrix Data Structure
;; =======================================================

;; === matrix struct ====
(defstruct (matrix (:constructor %make-matrix (rows cols data)))
  (rows 0 :type fixnum)
  (cols 0 :type fixnum)
  (data (null-pointer) :type foreign-pointer))

;; === create matrix ===
(defun make-matrix (rows cols &key (initial-element 0.0) (initial-contents nil))
  "Allocate array with designated size"
  (declare (type fixnum rows cols)
           (type single-float initial-element)
           (type (or null (simple-array single-float (*))) initial-contents))
  (let* ((size (* rows cols))
         (ptr (null-pointer)))
    (if initial-contents
        (progn
          (assert (= (length initial-contents) size) () "initial-contents size mismatch.")
          (setf ptr (foreign-alloc :float
                                   :count size
                                   :initial-contents initial-contents)))
        (setf ptr (foreign-alloc :float :count size :initial-element initial-element)))
    (%make-matrix rows cols ptr)))

;; === free matrix data ===
(defun free-matrix (mat)
  (declare (type matrix mat))
  (foreign-free (matrix-data mat))
  (setf (matrix-data mat) (null-pointer)))
    
;; === Accessor (getter) ===
(defun mref (mat row col)
  (mem-aref (matrix-data mat) :float (+ col (* row (* (matrix-cols mat))))))

;; === Accessor (setter) ===
(defun (setf mref) (val mat row col)
  (setf (mem-aref (matrix-data mat) :float (+ col (* row (matrix-cols mat))))
        (coerce val 'single-float)))

;; === Print ===
(defun print-matrix (mat &optional (limit 5))
  (format t "~%<Matrix ~dx~d>~%" (matrix-rows mat) (matrix-cols mat))
  (dotimes (i (min (matrix-rows mat) limit))
    (dotimes (j (min (matrix-cols mat) limit))
      (format t "~5,2f " (mref mat i j)))
    (if (> (matrix-cols mat) limit)
        (format t "...~%")
        (format t "~%")))
  (when (> (matrix-rows mat) limit) (format t "...~%")))


;; =======================================================
;;  Wrapper Functions
;; =======================================================

(defun sgemm! (A B C &key (alpha 1.0) (beta 0.0) (trans-a nil) (trans-b nil))
  "C = αA∙B + βC"
  (declare (type matrix A B C)
           (type single-float alpha beta))
  (let ((m (if trans-a (matrix-cols A) (matrix-rows A)))
        (n (if trans-b (matrix-rows B) (matrix-cols B)))
        (k-a (if trans-a (matrix-rows A) (matrix-cols A)))
        (k-b (if trans-b (matrix-cols B) (matrix-rows B)))
        (m-c (matrix-rows C))
        (n-c (matrix-cols C)))
    (assert (= k-a k-b) () "dimention of A and B mismatch.")
    (assert (= m m-c) () "dimention of A and C mismatch.")
    (assert (= n n-c) () "dimention of B and C mismatch.")
    (%cblas-sgemm +cblasRowMajor+
                  (if trans-a +CblasTrans+ +CblasNoTrans+)
                  (if trans-b +CblasTrans+ +CblasNoTrans+)
                  m n k-a
                  (coerce alpha 'single-float)
                  (matrix-data A) (matrix-cols A)
                  (matrix-data B) (matrix-cols B)
                  (coerce beta 'single-float)
                  (matrix-data C) (matrix-cols C))
    C))

(defun saxpy! (X Y &key (alpha 1.0))
  "Y = αX+Y (vector add)"
  (declare (type matrix X Y)
           (type single-float alpha))
  (let ((size-x (* (matrix-rows X) (matrix-cols X)))
        (size-y (* (matrix-rows Y) (matrix-cols Y))))
    (assert (= size-x size-y) () "vector size of X and Y mismatch.")
    (%cblas-saxpy size-x
                  (coerce alpha 'single-float)
                  (matrix-data X) 1
                  (matrix-data Y) 1)
    Y))

(defun sscal! (X alpha)
  "X = αX"
  (declare (type matrix X)
           (type single-float alpha))
  (%cblas-sscal (* (matrix-rows X) (matrix-cols X))
                (coerce alpha 'single-float)
                (matrix-data X) 1)
  X)

(defun clone-matrix (source)
  "clone matrix"
  (declare (type matrix source))
  (let* ((rows (matrix-rows source))
         (cols (matrix-cols source))
         (dest (make-matrix rows cols)))
    (%cblas-scopy (* rows cols)
                  (matrix-data source) 1
                  (matrix-data dest) 1)
    dest))

(defun copy-matrix! (source dest)
  "overwrite dest matrix with source matrix data"
  (declare (type matrix source dest))
  (assert (and (= (matrix-rows source) (matrix-rows dest))
               (= (matrix-cols source) (matrix-cols dest)))
          ()
          "matrix size of source and dest mismatch.")
  (%cblas-scopy (* (matrix-rows source) (matrix-cols source))
                (matrix-data source) 1
                (matrix-data dest) 1)
  dest)



;; =======================================================
;;  Extensions for Deep Learning
;; =======================================================

(defun fill-random! (mat &key (min -0.1) (max 0.1))
  "fill matrix with random values in the range"
  (declare (type matrix mat)
           (type single-float min max))
  (let ((range (- max min))
        (ptr (matrix-data mat))
        (size (* (matrix-rows mat) (matrix-cols mat))))
    (dotimes (i size)
      (setf (mem-aref ptr :float i)
            (coerce (+ min (random range)) 'single-float)))))

(defun matrix-map! (fn mat)
  "apply function to each element in matrix"
  (declare (type matrix mat))
  (let ((ptr (matrix-data mat))
        (size (* (matrix-rows mat) (matrix-cols mat))))
    (dotimes (i size)
      (setf (mem-aref ptr :float i)
            (coerce (funcall fn (mem-aref ptr :float i)) 'single-float))))
  mat)

(defun hadamard! (A B C)
  "C = A ⊙ B (element-wise product)"
  (declare (type matrix A B C))
  (let ((size (* (matrix-rows A) (matrix-cols A))))
    (assert (= (matrix-rows A) (matrix-rows B) (matrix-rows C))
            () "number of rows of A, B, C mismatch.")
    (assert (= (matrix-cols A) (matrix-cols B) (matrix-cols C))
            () "number of cols of A, B, C mismatch.")
    (dotimes (i size)
      (setf (mem-aref (matrix-data C) :float i)
            (* (mem-aref (matrix-data A) :float i)
               (mem-aref (matrix-data B) :float i)))))
  C)

(defun add-vector! (mat vec)
  "Mat(MxN) + Vec(1xN) (streach Vec to MxN)"
  (declare (type matrix mat vec))
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (p-mat (matrix-data mat)))
    (assert (= (matrix-cols mat) (matrix-cols vec))
            () "number of rows of mat and vec mismatch.")
    (dotimes (r rows)
      (%cblas-saxpy cols 1.0
                    (matrix-data vec) 1
                    (inc-pointer p-mat (* r cols 4)) 1)))
  mat)

(defun sum-all (mat)
  "sum up all the elements in mat"
  (declare (type matrix mat))
  (let ((sum 0.0)
        (ptr (matrix-data mat))
        (size (* (matrix-rows mat) (matrix-cols mat))))
    (dotimes (i size)
      (incf sum (mem-aref ptr :float i)))
    sum))

(defun argmax (mat &key (row 0))
  "return max element in designated row"
  (declare (type matrix mat)
           (type fixnum row))
  (let* ((val (mref mat row 0))
         (max-val val)
         (max-idx 0))
    (dotimes (c (matrix-cols mat))
      (setf val (mref mat row c))
      (when (> val max-val) (setf max-val val
                                  max-idx c)))
    max-idx))