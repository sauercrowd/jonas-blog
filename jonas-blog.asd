(asdf:defsystem "jonas-blog"
  :version "0.1.0"
  :author "Jonas Otten"
  :license ""
  :depends-on ("ningle" "clack" "hunchentoot" "bordeaux-threads")
  :components ((:module "src"
                :components
                ((:file "jonas-blog"))))
  :description "")
