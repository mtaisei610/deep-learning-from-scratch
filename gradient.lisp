(ql:quickload :vgplot)

(defmacro with-vec-updated ((vec i new) &body body)
  "vecのi番目の要素を値newで置き換えてbodyの処理を行ない、その後vecを元に戻す。"
  (let ((old (gensym "old")))
    `(let ((,old (aref ,vec ,i)))
       (unwind-protect
	    (progn (setf (aref ,vec ,i) ,new)
		   ,@body)
	 (setf (aref ,vec ,i) ,old)))))


(defun function-2 (xs)
  (declare (type (simple-array double-float (2)) xs))
  (+ (expt (aref xs 0) 2) (expt (aref xs 1) 2)))


;; 偏微分する(xsはsimple-arrayで、fの引数のxsと次元を合わせる)
(defun numerical-gradient (f xs)
  (declare (type (simple-array double-float (*)) xs))
  (let* ((len (length xs))
	(ret (make-array len :element-type 'double-float))
	(h 1d-4))
    (replace ret xs)
    (dotimes (i len)
      (let ((fxh1 0.0d0)
	    (fxh2 0.0d0)
	    (x (aref xs i)))
	(with-vec-updated (xs i (+ x h))
	  (setf fxh1 (funcall f xs)))
	(with-vec-updated (xs i (- x h))
	  (setf fxh2 (funcall f xs)))
	(setf (aref ret i)
	      (/ (- fxh1 fxh2) (* 2 h)))))
    ret))

  
;; 勾配降下
(defun gradient-descent (f init-xs lr step)
  (declare (type (simple-array double-float (*)) init-xs))
  (let ((xs (make-array (length init-xs) :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) xs))
    (replace xs init-xs)
    (dotimes (i step)
      (map-into xs (lambda (x g) (- x (* lr g))) xs (numerical-gradient f xs)))
    xs))
