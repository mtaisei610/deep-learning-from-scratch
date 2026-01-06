(defsystem "deep-learning-from-scratch"
	   :version "0.1.0"
	   :author "Taisei Muto"
	   :license "MIT"
	   :depends-on ("cffi" "vgplot")
	   :serial t
	   :components ((:file "matrix-lib")
			(:file "functions")
			(:file "gradient")))
