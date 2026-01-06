(defpackage :two-layer-net
  (:use :cl :cffi))

(in-package :two-layer-net)

(defclass two-layer-net ()
  ((params :initform (make-hash-table :test 'equal)
           :accessor params))
