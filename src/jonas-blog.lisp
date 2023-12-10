(defpackage jonas-blog
  (:use :cl :ningle :clack :bordeaux-threads))

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

(create-html-tags ("h1" "h2" "h3" "html" "script" "img" "a" "head" "body" "meta" "div" "span" "p"))




(defun generate-body ()
  (div '(:class "flex w-screen h-screen")
       (div
	'(:class "bg-slate-300 w-64 items-center flex flex-col p-2 pt-4 gap-2")
	(img '(:class "w-32 h-32 rounded-full" :src "https://avatars.githubusercontent.com/u/4764029?v=4"))
	(h1 "Jonas Otten")
	(p '(:class "text-sm w-48")
	 "Software engineer at "
	    (a '(:href "https://loxoapp.com") "Loxo")
	  " previously at " (a '(:href "https://palantir.com") "Palantir")))
       (div "hello this is joans blog")))

(defun handle-root (&rest args)
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
		    (body (generate-body)))))


(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/") #'handle-root)

(defun main()
  (let ((server (clack:clackup *app* :address "0.0.0.0" :port 8080)))
    (handler-case (bordeaux-threads:join-thread (find-if (lambda (th)
							   (search "hunchentoot" (bordeaux-threads:thread-name th)))
							 (bordeaux-threads:all-threads)))
      ;; Catch a user's C-c
      (#+sbcl sb-sys:interactive-interrupt
       () (progn
	    (format *error-output* "Aborting.~&")
	    (clack:stop server)
	    (uiop:quit)))
      (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))))
