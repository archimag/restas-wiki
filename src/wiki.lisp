;;; wiki.lisp
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:restas.wiki)

;;;; show page  

(defun show-wiki-page (page)
  (finalize-page 
   (let ((path (wiki-page-pathname page))
         (epage (hunchentoot:url-decode page)))
     (if (fad:file-exists-p path)
         (restas.wiki.view:show-page (list :page (generate-page-html (wiki-parser:parse :dokuwiki path))
                                           :menu (if (wiki-user)
                                                     `((:action "edit" :href ,(restas:genurl 'edit-wiki-page :page epage))
                                                       (:action "history" :href ,(restas:genurl 'history-wiki-page :page epage))
                                                       (:action "pdf" :href ,(restas:genurl 'view-wiki-page-in-pdf :page epage))))))
         (restas.wiki.view:page-not-found `(:create-link ,(restas:genurl 'edit-wiki-page :page epage)))))
   (hunchentoot:url-decode page)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; routes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *resource-dir*
  (merge-pathnames "resource/"
                   (asdf:component-pathname (asdf:find-system '#:restas-wiki))))

(define-route css ("css/:file")
  (merge-pathnames (format nil "css/~A" file)
                   *resource-dir*))


(define-route wiki-main-page ("")
  (show-wiki-page "index"))

(define-route view-wiki-page-in-pdf (":(page)/pdf"
                                            :content-type "application/pdf")
  (flexi-streams:with-output-to-sequence (out)
    (let ((out* (flexi-streams:make-flexi-stream out)))
      (pdf-render-wiki-page (wiki-parser:parse :dokuwiki
                                               (wiki-page-pathname page))
                            out*))
    out))

(define-route view-wiki-page (":(page)")
  (show-wiki-page page))


(define-route edit-wiki-page ("edit/:(page)"                                     
                              :requirement #'wiki-user)
  (finalize-page 
   (restas.wiki.view:edit-page (list :page-content (if (fad:file-exists-p (wiki-page-pathname page))
                                                       (alexandria:read-file-into-string (wiki-page-pathname page)))))
   (format nil "Редактирование ~A" (hunchentoot:url-decode page))))

(defun save-wiki-page (page content author &optional comment)
  (let* ((time (get-universal-time))
         (page-path (ensure-directories-exist (wiki-page-pathname page)))
         (changes-path (ensure-directories-exist (wiki-page-changes-pathname page)))
         (archive-path (ensure-directories-exist (wiki-page-archive-pathname page time)))
         (changes (nconc (if (fad:file-exists-p changes-path)
                          (with-open-file (in changes-path)
                            (with-standard-io-syntax
                              (read in))))
                      (list (list time
                                  author
                                  (if (fad:file-exists-p page-path)
                                      :edit
                                      :create)
                                  (hunchentoot:url-decode page)
                                  comment)))))
    (with-open-file (out changes-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax
        (print changes
               out)))
    (write-string-into-gzip-file content archive-path)
    (alexandria:write-string-into-file content
                                       page-path
                                       :if-exists :supersede
                                       :if-does-not-exist :create)))

(define-route edit-wiki-page/post ("edit/:(page)"
                                   :method :post
                                   :requirement #'wiki-user)
  (cond
    ((hunchentoot:post-parameter "cancel") (restas:redirect 'view-wiki-page 
                                                            :page page))
    ((hunchentoot:post-parameter "preview") (finalize-page
                                             (restas.wiki.view:edit-page
                                              (list :page-content (hunchentoot:post-parameter "page-content")
                                                    :preview (generate-page-html
                                                              (wiki-parser:parse :dokuwiki
                                                                                 (hunchentoot:post-parameter "page-content")))))
                                             (format nil "Предпросмотр ~A" (hunchentoot:url-decode page))))
    (t (progn
         (save-wiki-page page
                         (hunchentoot:post-parameter "page-content")
                         (wiki-user))
         (restas:redirect 'view-wiki-page
                          :page page)))))
                                     

(define-route history-wiki-page ("history/:(page)"
                                 :requirement #'wiki-user)
  (flet ((version-plist (item)
           (list :date (local-time:format-timestring nil
                                                     (local-time:universal-to-timestamp (first item))
                                                     :format '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\Space (:HOUR 2) #\: (:MIN 2)))
                 :href (restas:genurl 'view-archive-wiki-page
                                      :page (hunchentoot:url-decode page)
                                      :time (first item))
                 :name (fourth item)
                 :author (second item))))
    (let* ((change-path (wiki-page-changes-pathname page))
           (changes (nreverse (if (fad:file-exists-p change-path)
                                  (with-open-file (in change-path)
                                    (with-standard-io-syntax
                                      (read in)))))))
      (finalize-page
       (restas.wiki.view:history-page `(:menu ((:action "view" :href ,(restas:genurl 'view-wiki-page :page (hunchentoot:url-decode page))))
                                        :history ,(mapcar #'version-plist changes)))
       (format nil "История ~A" (hunchentoot:url-decode page))))))

    
(define-route view-archive-wiki-page ("history/:(page)/:(time)")
  (finalize-page 
   (let* ((path (wiki-page-archive-pathname page time))
          (dpage (hunchentoot:url-decode page))
          (menu (if (wiki-user)
                    `((:action "current" :href ,(restas:genurl 'view-wiki-page :page dpage))
                      (:action "history" :href ,(restas:genurl 'history-wiki-page :page dpage))))))
     (if (fad:file-exists-p path)
         (restas.wiki.view:show-page (list :menu menu
                                           :page (generate-page-html (wiki-parser:parse :dokuwiki
                                                                                        (read-gzip-file-into-string path)))))
         (restas.wiki.view:archive-not-found (list :menu menu))))
   (format nil "Архивная версия - ~A" (hunchentoot:url-decode page))))

