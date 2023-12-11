(in-package :jonas-blog)


(defun get-date-from-post-filename (filename)
  (ignore-errors
   (local-time:parse-timestring
    (car (cl-ppcre:split "_" filename)))))

(defun get-post-files ()
  (loop for f in (directory "*.md")
		 if (or
		     (get-date-from-post-filename (file-namestring f))
		     (cl-ppcre:scan "\.md$" (file-namestring f)))
		   collect (file-namestring f)))


(defclass post ()
  ((title :initarg :title
	  :accessor title)
   (date :initarg :date
	 :accessor date)
   (content :initarg :content
	     :accessor content)
   (path :initarg :path
	 :accessor path)))



(defun load-post (filename)
  (let* ((splitted (cl-ppcre:split "_" filename))
	 (filename-without-ext (car (cl-ppcre:split "\\." (cadr splitted)))))
    (make-instance 'post
		   :title (cl-ppcre:regex-replace-all "-" filename-without-ext " ")
		   :date (local-time:parse-timestring (car splitted))
		   :path filename-without-ext
		   :content (with-open-file (stream filename)
			      (loop for line = (read-line stream nil)
				    while line
				    collect line)))))

(defun get-posts ()
  (mapcar #'load-post (get-post-files)))

(defun get-posts-table ()
  (let ((posts-table (make-hash-table)))
    (dolist (post (get-posts))
      (setf (gethash (path post)) post))
    (return posts-table)))
