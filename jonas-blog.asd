(asdf:defsystem "jonas-blog"
  :version "0.1.0"
  :author "Jonas Otten"
  :license ""
  :depends-on (:woo :local-time :cl-ppcre :sqlite :sb-md5)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "markdown")
		 (:file "jonas-blog"))))
  :description "")
