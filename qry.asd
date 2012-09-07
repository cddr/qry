

(defsystem :qry-test
  :serial t
  :depends-on (cl-test-more)
  :components ((:file "tests")))

(defsystem :qry
  :description "Common Lisp implementation of tnetstrings http://tnetstrings.org/"
  :serial t
  :depends-on (:postmodern :s-xml :hunchentoot)
  :components ((:file "package")
               (:file "qry")
               (:file "web"))
  :in-order-to ((test-op (load-op cl-tnetstrings-test)))
  :perform (test-op :after (op c)
             (funcall
              (intern (format nil "~a" 'run-test-package)
                      (find-package :cl-test-more))
              :qry-test)))

