(asdf:defsystem "jonas-blog"
  :version "0.1.0"
  :author "Jonas Otten"
  :license ""
  :depends-on (:woo :local-time :cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "jonas-blog"))))
  :description "")
