
(in-package :qry)

(defvar *qry-server* nil)

(defparameter *js-libs*
  (list "//netdna.bootstrapcdn.com/twitter-bootstrap/2.1.0/js/bootstrap.min.js"
        "http://code.jquery.com/jquery-1.8.1.min.js"
        "//cdnjs.cloudflare.com/ajax/libs/jqueryui/1.8.23/jquery-ui.min.js"
        "/js/qry.js"))

(defparameter *css-libs*
  (list "//netdna.bootstrapcdn.com/twitter-bootstrap/2.1.0/css/bootstrap-combined.min.css"
        "/css/qry.css"))

(defun include-css-libs ()
  (loop for lib in *css-libs*
       collect `(:link (:@ (:rel "stylesheet") (:href ,lib)) "")))

(defun include-js-libs ()
  (loop for lib in *js-libs*
       collect `(:script (:@ (:src ,lib) (:type "text/javascript")) "")))
 
(defun handle-qry (qry qry-def)
  (let ((*qry* qry))
    (let ((reply `(:html
                   (:head
                    ,@(include-css-libs)
                    ,@(include-js-libs))
                   (:body
                    ,(%ui-eval (qry-def-query-ui qry-def))))))
      (setf (tbnl:content-type*) "text/html")
      (s-xml:print-xml-string reply :pretty t
                              :input-type :sxml
                              :header "<!DOCTYPE HTML>"
                              :downcase-identifiers t))))
  
(defun qry-dispatcher (req)
  (let* ((pname (pathname (tbnl:script-name req)))
         (sym (read-from-string (pathname-name pname))))
    (lambda ()
      (let* ((qry-def (get sym :qry-def))
             (qry (make-qry :def qry-def
                            :params (tbnl:get-parameters req))))
        (handle-qry qry qry-def)))))

(defun start-query-server (&key port)
  (cfg-qry-server)
  (setf *qry-server* (tbnl:start (make-instance 'tbnl:easy-acceptor :port port))))

(defun stop-query-server ()
  (when *qry-server*
    (tbnl:stop *qry-server*)
    (setf *qry-server* nil)))


(defun cfg-qry-server ()
  (flet ((resource-dir (relative-path)
           "relative path should be a list of paths leading to the intended resource directory"
           (let ((dir (merge-pathnames
                       (make-pathname :directory `(:relative ,@relative-path))
                       (make-pathname :name nil :type nil
                                      :defaults #.(or *compile-file-truename* *load-truename*)))))
             (print dir)
             dir)))
    (setf tbnl:*dispatch-table*
          (list 
           (tbnl:create-folder-dispatcher-and-handler "/js/" (resource-dir '("static" "js")))
           (tbnl:create-folder-dispatcher-and-handler "/css/" (resource-dir '("static" "css")))
           'qry-dispatcher))))
