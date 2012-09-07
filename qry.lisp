
(in-package :qry)

(defstruct qry-def
  predicate
  query-view
  result-view
  query-ui
  result-ui)

(defstruct qry
  def
  params)

(defvar *qry*)
(defvar *queries* (make-hash-table))

(defun %qry-eval (form)
  (cond 
    ((null form) nil)
    ((symbolp form) (let ((val (getf (qry-params *qry*) form)))
                      (typecase val
                        (null t)
                        (atom `(:= ,form ,val))
                        (list `(:in ,form ,val)))))
    ((listp form) (destructuring-bind (op &rest rest)
                      form
                    (case op
                      (and `(:and ,@(mapcar '%qry-eval rest)))
                      (or `(:or ,@(mapcar '%qry-eval rest))))))))

(defun qry-eval (qry)
  (let* ((*qry* qry)
         (def (qry-def qry))
         (predicate (%qry-eval (qry-def-predicate def))))
    (s-sql:sql-compile
     `(:select *
         :from ,(qry-def-result-view def)
         :where (:in id (:select id
                           :from ,(qry-def-query-view def)
                           :where ,predicate))))))



(defun to-string (sym)
  (string-downcase (format nil "~a" sym)))

(defun include-attrs (attrs)
  (when attrs
    `(:@ ,@(loop with coll = t
              for (attr . rest) on '(:foo 1 :bar )
              when coll
              collect `(,attr ,(if (keywordp (car rest))
                                   (string-downcase attr)
                                   (prog1 (format nil "~a" (car rest))
                                     (setf coll nil))))))))

(defvar *selected*)

(defun %ui-eval (form)
  (cond 
    ((null form) nil)
    ((stringp form) form)
    (t (destructuring-bind (op &rest args)
           form
         (case op
           (date-field (destructuring-bind (name label)
                           args
                         (%ui-eval
                          `(div (:class "control-group")
                             (label (:class "control-label" :for ,(to-string name)) ,label)
                             (div (:class "controls")
                               (input (:type "date"
                                             :class "date"
                                             :name ,(to-string name)
                                             :value ,(getf (qry-params *qry*) name))))))))
           (integer-field (destructuring-bind (name label)
                              args
                            (%ui-eval
                             `(div (:class "control-group")
                                (label (:class "control-label" :for ,(to-string name)) ,label)
                                (input (:type "integer"
                                              :name ,(to-string name)
                                              :value ,(getf (qry-params *qry*) name)))))))
           (select-field (destructuring-bind ((&rest attrs) name label &body body)
                             args
                           (let ((*selected* (getf (qry-params *qry*) name)))
                             (%ui-eval
                              `(div (:class "control-group")
                                 (label (:class "control-label" :for ,(to-string name)) ,label)
                                 (select ,(include-attrs attrs)
                                   ,@body))))))
           (option (let ((option (car args)))
                     `(option ,@(when (member option *selected*) '(:selected))
                        ,option)))
           (otherwise (destructuring-bind ((&rest attrs) &rest body)
                          args
                        `(,(intern (string op) :keyword)
                           ,@(include-attrs attrs)
                           ,@(mapcar '%ui-eval body)))))))))

(defun list-states ()
  (list "FL" "KY" "TX"))

#+nil(let* ((*qry* (make-qry))
      (markup (%ui-eval `(form
                          (fieldset
                           (hbox (vbox (date date-from "From")
                                       (date date-to "Thru"))
                                 (vbox "OR")
                                 (vbox (integer year "Year")
                                       (select quarter "Quarter" :from ,(mapcar 'to-string (list 1 2 3 4))))))
                           
                          (fieldset
                           (legend "Insured")
                           (vbox (integer insured "ID or SSN"))
                           (vbox (select state "State" :from ,(list-states))))))))
       (print markup)
       (s-xml:print-xml-string markup :pretty t :input-type :sxml :downcase-identifiers t))

(defmacro defquery (name (&rest rest) &body body)
  (declare (ignore rest))
  `(let ((sym ',name))
     (setf (get sym :qry-def) (make-qry-def ,@body)
           (gethash ',name *queries*) sym)))


(defquery notes-log (date-from date-to year quarter member state user 
                               note-type note-type-modifier)
  :predicate `(:and (:or (:and (:>= date-from)
                               (:<= date-to)
                         (:and year quarter))
                    member state user
                    note-type modifier))
  :query-view 'notes-query
  :result-view 'notes-log
  :query-ui `(form (:class "form-horizontal")
               (fieldset ()
                 (legend "Date")
                 (date date-from "From")
                 (date date-to "Thru")
                 (div (:class "vbox center")
                   "OR")
                 (integer year "Year")
                 (select quarter "Quarter" :from ,(mapcar 'to-string '(1 2 3 4))))

               (fieldset ()
                 (legend "Insured")
                 (integer insured "ID/SSN")
                 (select state "State" :from ,(list-states))))
  :result-ui 'slick-grid)
               

