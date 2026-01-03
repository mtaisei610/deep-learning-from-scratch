(defpackage :mnist-loader
  (:use :cl)
  (:export :mnist-loader))
(in-package :mnist-loader)


(defun read-u32-be (stream)
  (let ((b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream))
        (b4 (read-byte stream)))
    (logior (ash b1 24)
            (ash b2 16)
            (ash b3 8)
            b4)))

(defun make-one-hot (label class-count)
  (let ((arr (make-array class-count :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref arr label) 1.0d0)
    arr))


(defun load-images (path)
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let ((magic (read-u32-be s))
          (count (read-u32-be s))
          (rows  (read-u32-be s))
          (cols (read-u32-be s)))
      (format t "Loading Images: ~a (cound: ~d, Size:~dx~dx)~%" path count rows cols)

      (let ((images (make-array count)))
        (dotimes (i count)
          (let ((pixels (make-array (* rows cols) :element-type 'double-float)))
            (dotimes (p (* rows cols))
              (setf (aref pixels p) (/ (read-byte s) 255.0d0)))
            (setf (aref images i) pixels)))
        images))))


(defun load-labels (path)
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let ((magic (read-u32-be s))
          (count (read-u32-be s)))
      (format t "Loading Labels: ~a (Count: ~d)~%" path count)
      (let ((labels (make-array count)))
        (dotimes (i count)
          (let ((raw-label (read-byte s)))
            (setf (aref labels i) (make-one-hot raw-label 10))))
        labels))))


(defun load-dataset (image-path label-path)
  (values (load-images image-path)
          (load-labels label-path)))


;; test

(defparameter *base-dir* "/home/mt/workspace/school/deep-learning-from-scratch/data/mnist/")

(let ((img-file (concatenate 'string *base-dir* "t10k-images-idx3-ubyte"))
      (lbl-file (concatenate 'string *base-dir* "train-labels-idx1-ubyte")))
  (if (and (probe-file img-file) (probe-file lbl-file))
      (multiple-value-bind (images labels) (load-dataset img-file lbl-file)
        (format t "Done!~%")
        (format t "Sample Image[0] (first 5 pixels): ~a...~%" (subseq (aref images 0) 0 5))
        (format t "Sample Label[0] (One-Hot): ~a~%" (aref labels 0)))
      (format t "MNIST files not found:~%~a~%~a~%" img-file lbl-file)))
