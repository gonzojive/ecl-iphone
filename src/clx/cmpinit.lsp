(defvar std-compile (symbol-function 'compile-file))
(defun compile-file (file &rest args &key (output-file 'T))
  (funcall std-compile
	   file
	   :c-file t :h-file t :data-file t
	   :output-file output-file))

