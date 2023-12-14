(in-package :jonas-blog)

(defun get-date-from-post-filename (filename)
  (ignore-errors
   (local-time:parse-timestring
    (car (cl-ppcre:split "_" filename)))))

(defun get-post-files ()
  (loop for f in (directory (format nil "~a/*.md" (asdf:system-relative-pathname :jonas-blog ".")))
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
		   :path (string-downcase filename-without-ext)
		   :content (format nil "~{~A~^~% ~}"
				    (with-open-file (stream (asdf:system-relative-pathname :jonas-blog filename))
				      (loop for line = (read-line stream nil)
					    while line
					    collect line))))))

(defun get-posts ()
  (mapcar #'load-post (get-post-files)))

(defun get-posts-table ()
  (let ((posts-table (make-hash-table :test 'equal)))
    (dolist (post (get-posts))
      (setf (gethash (format nil "/~a" (path post)) posts-table) post))
    posts-table))



(defmacro -> (&rest args)
  (reduce (lambda (acc current) `(,current ,acc))
	  (subseq args 2)
	  :initial-value `(,(cadr args) ,(car args))))


(defun parse-header (content)
  (cl-ppcre:regex-replace-all "[ ]*(#+) (.*)"
			      content
			      (lambda (match &rest registers)
				(let ((header-size (car registers))
				      (header-content (cadr registers)))
				  (cond ((string= header-size "#") (h1 '(:class "text-3xl mt-4") header-content))
					((string= header-size "##") (h2 '(:class "text-2xl mt-2") header-content))
					((string= header-size "###") (h3 '(:class "text-xl mt-2") header-content))
					(t (b header-content)))))
			      :simple-calls t))


(defun parse-paragraphs (content)
  (cl-ppcre:regex-replace-all "\\n[ ]*\\n" content "<br />"))

(defun parse-links (content)
  (cl-ppcre:regex-replace-all " \\[(.*)\\]\\((.*)\\)" content
			      (lambda (match &rest registers)
				(a `(:class "mx-1 underline" :href ,(cadr registers)) (car registers))) :simple-calls t))

(defun parse-images (content)
  (cl-ppcre:regex-replace-all " !\\[(.*)\\]\\((.*)\\)" content
			      (lambda (match &rest registers)
				(div
				 (img `(:class "rounded-sm max-w-[512px] max-h-[512px]" :src ,(cadr registers) :alt ,(car registers))))) :simple-calls t))


(defun parse-inline-code (content)
  (cl-ppcre:regex-replace-all "[\\s]`(.*?)`" content
			      (lambda (match &rest registers)
				(span '(:class "m-1 rounded-sm bg-slate-100 text-black p-1") (car registers)))
			      :simple-calls t))

(defun parse-code-blocks (content)
  (cl-ppcre:regex-replace-all (create-scanner "```.*?\\n((?:.|\\s)*?)```"  :multi-line-mode t) content
			      (lambda (match &rest registers)
				(div '(:class "m-1 rounded-sm bg-slate-100 text-black p-1") (car registers)))
			      :simple-calls t))
  
  

(defun markdown-to-html (markdown-content)
  (div '(:class "max-w-[1024px]" :id "blog-content")
       (-> markdown-content
	   parse-code-blocks
	   parse-inline-code
	   parse-links
	   parse-images
	   parse-header
	   parse-paragraphs)))
