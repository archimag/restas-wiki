;;;; drawer.lisp
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.wiki)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric page-finalizator (drawer)
  (:documentation "Return page finalizator"))

(defgeneric render-route-data (drawer data route )
  (:documentation "Render page for specific route"))

(defgeneric generate-body-content (drawer data)
  (:documentation "Generate content of body"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; trivial implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drawer () ())

(defmethod restas:render-object ((drawer drawer) (data list))
  (funcall (page-finalizator drawer)
           (list* :body (render-route-data drawer
                                           data
                                           (restas:route-symbol restas:*route*))
                  data)))

(defmethod page-finalizator ((drawer drawer))
  'restas.wiki.view:finalize-page)

(defmethod generate-body-content ((drawer drawer) data)
  (render-wiki-page-to-string (wiki-parser:parse :dokuwiki (getf data :body))))

(defmethod render-route-data ((drawer drawer) data route)
  (funcall (find-symbol (symbol-name route)
                      '#:restas.wiki.view)
           data))

(defmethod render-route-data ((drawer drawer) data (route (eql 'main-wiki-page)))
  (render-route-data drawer data 'show-wiki-page))

(defmethod render-route-data ((drawer drawer) data (route (eql 'show-wiki-page)))
  (if (getf data :body)
      (generate-body-content drawer data)
      (restas.wiki.view:page-not-found (list :create-link
                                             (restas:genurl 'edit-wiki-page
                                                            :page (getf data :title))))))

(defmethod render-route-data ((drawer drawer) data (route (eql 'edit-wiki-page/preview)))
  (render-route-data drawer
                    (list* :preview (generate-body-content drawer data)
                           data)
                    'edit-wiki-page))


(defparameter *date-format* '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\Space (:HOUR 2) #\: (:MIN 2)))

(defmethod render-route-data ((drawer drawer) data (route (eql 'history-wiki-page)))
  (call-next-method drawer
                    (list :title (getf data :title)
                          :history (iter (for item in (getf data :history))
                                         (collect (list* :date
                                                         (local-time:format-timestring nil
                                                                                       (local-time:universal-to-timestamp (getf item :date))
                                                                                       :format *date-format*)
                                                         item))))
                    route))

(defmethod render-route-data ((drawer drawer) data (route (eql 'show-archive-wiki-page)))
  (generate-body-content drawer data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; use drawer default
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *default-render-method* (make-instance 'drawer))