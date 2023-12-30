(defpackage jonas-blog
  (:use :cl :woo :local-time :cl-ppcre :sqlite)
  (:export :main-fn))

(loop for f in (uiop:subdirectories "/")
		collect (format t "directory at root ~a~%" f))
