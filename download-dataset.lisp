(defpackage :mnist-downloader
  (:use :cl :uiop))
(in-package :mnist-downloader)

(defparameter *mnist-base-url*
  "https://storage.googleapis.com/cvdf-datasets/mnist/")

(defparameter *mnist-files*
  '("train-images-idx3-ubyte.gz"
    "train-labels-idx1-ubyte.gz"
    "t10k-images-idx3-ubyte.gz"
    "t10k-labels-idx1-ubyte.gz"))

(defparameter *mnist-dir* "data/mnist/")

(defun ensure-dir (dir)
  (ensure-directories-exist dir))

(defun run (cmd)
  (format t "~a~%" cmd)
  (run-program cmd :output t :error-output t))

(defun download-mnist ()
  (ensure-dir *mnist-dir*)
  (with-current-directory (*mnist-dir*)
    (dolist (file *mnist-files*)
      (run (list "curl" "-O"
                  (concatenate 'string *mnist-base-url* file)))
      (run (list "gzip" "-d" "-f" file))))
  (format t "MNIST download complete.~%"))

