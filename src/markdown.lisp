(in-package :jonas-blog)


(defun get-date-from-post-filename (filename)
  (ignore-errors
   (local-time:parse-timestring
    (car (cl-ppcre:split "_" filename)))))

(defun list-posts ()
  (loop for f in (directory "*.md")
		 if (or
		     (get-date-from-post-filename (file-namestring f))
		     (cl-ppcre:scan "\.md$" (file-namestring f)))
		   collect f))

