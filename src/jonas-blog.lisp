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

(create-html-tags ("h1" "h2" "h3" "html" "script" "img" "a" "head" "body" "meta" "div" "span" "p" "b"))



(defun sidebar-show-posts ()
  (mapcan (lambda (post)
	    (a (list
		:class "underline"
		:href (format nil "/~a" (path post)))
	       (title post)))
	  (get-posts)))

(defun generate-body (inner)
  (div '(:class "flex w-screen h-screen text-white")
       (div
	'(:class "bg-slate-800 w-64 items-center flex flex-col p-2 pt-4 gap-2")
	(img '(:class "w-32 h-32 rounded-full" :src "https://avatars.githubusercontent.com/u/4764029?v=4"))
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
		     (script '(:src "https://unpkg.com/htmx.org@1.9.9"
			       :integrity "sha384-QFjmbokDn2DjBjq+fM+8LUIVrAgqcNW2s0PjAxHETgRn9l4fvX31ZxDxvwQnyMOX"
			       :crossorigin "anonymous"))
		     (script '(:src "https://cdn.tailwindcss.com") ))
		    (body '(:class "bg-slate-700" :hx-boost "true") (generate-body inner)))))

(defun main-handler (env)
  (let ((req-path  (getf env :path-info)))
    (print req-path)
    (if (string= "/" req-path)
      (list 200 '(:content-type "text/html") (list (get-blog "")))
      (let ((post (gethash req-path *posts-table*)))
	(format t "~a ~a" post req-path)
	(if post
	    (list 200 '(:content-type "text/html") (list (get-blog (markdown-to-html (content post)))))
	    '(404 '(:content-type "text/plain") ("Not found")))))))


(defvar *app* (lambda (env)
		(main-handler env)))

(defun main ()
  (woo:run *app* :port 8080 :address "0.0.0.0"))

(defun dev ()
  (require 'bordeaux-threads)
  (bt:make-thread (lambda ()
		    (main))))
