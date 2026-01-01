(defun cross-entropy-error (y c)
  (let ((delta 1e-7)
        (sum 0.0))
    (dotimes (i (length y) (- sum))
      (incf sum
            (* (aref c i)
               (log (+ (aref y i) delta)))))))
