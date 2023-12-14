(in-package :jonas-blog)

(defun content-to-string (content)
  (if (stringp content)
      content
      (format nil "~{~A~^ ~}" content)))

(defun tag-key-to-string (k)
  (if (stringp k)
      k
      (string-downcase (symbol-name k))))

(defun create-tags (args)
  (let* ((k (car args))
	 (remaining-list (cdr args))
	 (v (car remaining-list)))
    (if v
	(format NIL "~a=\"~a\" ~a" (tag-key-to-string k) v (create-tags
					 (cdr remaining-list)))
	"")))


(defmacro create-html-tag (name)
  (let ((fn-name (read-from-string name)))
  `(defun ,fn-name (&rest args)
     (if (stringp (car args))
	(format nil "<~a>~a</~a>" ,name (content-to-string args)  ,name)
	(format nil "<~a ~a>~a</~a>" ,name (create-tags (car args)) (content-to-string (cdr args))  ,name)))))


(defmacro create-html-tags (tag-list)
  `(progn
     ,@(loop for tag in tag-list
	      collect `(create-html-tag ,tag))))

(create-html-tags ("h1" "h2" "h3" "html" "script" "img" "a" "head" "body" "meta" "div" "span" "p" "b" "link"))


(defun concat-string-list (str-list)
  (format nil "~{~a~}" str-list))

(defun sidebar-show-posts ()
  (concat-string-list
	       (mapcar (lambda (post)
			 (a (list
			     :class "underline"
			     :hx-trigger "click"
			     :hx-swap "innerHTML"
			     :hx-target "#blog-content"
			     :hx-get (format nil "/~a" (path post)))
			    (title post)))
		       (get-posts))))

(defun generate-body (inner)
  (div '(:class "flex w-screen h-screen text-white" :id "sidebar" )
       (div
	'(:class "bg-slate-800 w-64 flex-shrink-0 items-center flex flex-col p-2 pt-4 gap-2")
	(img '(:class "w-32 h-32 rounded-full" :src "https://avatars.githubusercontent.com/u/4764029?v=4" :id "profile-photo" :hx-preserve ""))
	(h1 "Jonas Otten")
	(p '(:class "text-sm w-48")
	 "Software engineer at "
	    (a '(:class "underline" :href "https://loxoapp.com") "Loxo")
	    " previously at " (a '(:class "underline" :href "https://palantir.com") "Palantir"))
	(sidebar-show-posts))
       inner))

(defun get-blog (inner)
      (concatenate 'string
		   "<!doctype html>"
		   (html
		    (head
		     (meta '(:charset "utf8"))
		     (meta '(:name "viewport" :content "width=device-width, initial-scale=1.0"))
		     (script '(:src "/static/htmx.min.js"))
		     (link '(:type "text/css" :href "/static/tailwind.css" :rel "stylesheet" )))
		    (body '(:class "bg-slate-700" :hx-boost "true")
			  (generate-body inner)))))

(defun get-matching-blog-post (req-path)
  (if (string= "/" req-path)
      ""
      (let ((post (gethash req-path *posts-table*)))
	(if post
	    (content post)
	    nil))))


(defun read-static-file (path)
  (with-open-file (file path :element-type '(unsigned-byte 8))
  (let ((data (make-array (file-length file) :element-type '(unsigned-byte 8))))
    (read-sequence data file)
    data)))
 

(defun get-static-assets ()
    (loop for f in (directory (format nil "~a/static/*.*" (asdf:system-relative-pathname :jonas-blog ".")))
	  collect
	    (list (concatenate 'string "/" (subseq
		   (namestring f)
		   (length
		    (namestring
		     (asdf:system-relative-pathname :jonas-blog "")))))
		  (read-static-file f))))


(defun get-static-asset-table ()
    (let ((static-item-table (make-hash-table :test 'equal)))
	(dolist (item (get-static-assets))
	(setf (gethash (car item) static-item-table) (cadr item)))
	static-item-table))


(defun serve-static-asset (env)
  (let* ((req-path  (getf env :path-info))
	 (static-asset (gethash req-path *static-assets*)))
    (if static-asset
	`(200 () ,static-asset) 
	'(404 (:content-type "text-plain") ("Not found")))))


(defun main-handler (env)
    (let* ((req-path  (getf env :path-info))
	    (is-htmx-req (gethash "hx-request"(getf env :headers)))
	    (maybe-post (get-matching-blog-post req-path)))
    (if maybe-post
	(if is-htmx-req
	    `(200 '(:content-type "text/html") (,(markdown-to-html maybe-post)))
	    `(200 '(:content-type "text/html") (,(get-blog (markdown-to-html maybe-post)))))
	(serve-static-asset env))))



(defvar *app* (lambda (env)
		(main-handler env)))

(defvar *posts-table* nil)
(defvar *static-assets* nil)

(setf *static-assets* (get-static-asset-table))
(setf *posts-table* (get-posts-table))

(defun main ()
  (woo:run *app* :port 8080 :address "0.0.0.0"))

(defun dev ()
  (require 'bordeaux-threads)
  (bt:make-thread (lambda ()
		    (main))))
