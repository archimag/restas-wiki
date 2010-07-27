;;; wiki.lisp
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:restas.wiki)

(define-route main-wiki-page ("")
  (show-wiki-page :page "index"))

(define-route show-wiki-page (":(page)")
  (list :title page
        :body (storage-find-page *storage* page)))  

(define-route edit-wiki-page ("edit/:(page)"                                     
                              :requirement #'wiki-user)
  (list :title (format nil "Edit \"~A\"" page)
        :content (if (fad:file-exists-p (wiki-page-pathname page))
                     (alexandria:read-file-into-string (wiki-page-pathname page)))))

(define-route edit-wiki-page/preview ("edit/:page"
                                      :method :post
                                      :requirement (lambda () (hunchentoot:post-parameter "preview")))
  (list :title page
        :content (hunchentoot:post-parameter "content")))

(define-route edit-wiki-page/cancel ("edit/:page"
                                     :method :post
                                     :requirement (lambda () (hunchentoot:post-parameter "cancel")))
  (restas:redirect 'show-wiki-page
                   :page page))

(define-route edit-wiki-page/save ("edit/:page"
                                   :method :post
                                   :requirement (lambda () (hunchentoot:post-parameter "save")))
  (storage-save-page *storage*
                     page
                     (hunchentoot:post-parameter "content")
                     (wiki-user))
  (restas:redirect 'show-wiki-page
                   :page page))

(define-route history-wiki-page ("history/:(page)"
                                 :requirement #'wiki-user)
  (list :title (format nil "History of page \"~A\"" page)
        :history (iter (for item in (storage-page-history *storage* page))
                       (collect (list* :href (restas:genurl 'show-archive-wiki-page
                                                            :page page
                                                            :revision (getf item :date))
                                       item)))))

(define-route show-archive-wiki-page ("history/:(page)/:(revision)")
  (list :title ""
        :body (storage-page-revision *storage* page revision)))
  
  ;; (finalize-page nnnnn
  ;;  (let* ((path (wiki-page-archive-pathname page time))
  ;;         (dpage page)
  ;;         (menu (if (wiki-user)
  ;;                   `((:action "current" :href ,(restas:genurl 'show-wiki-page :page dpage))
  ;;                     (:action "history" :href ,(restas:genurl 'history-wiki-page :page dpage))))))
  ;;    (if (fad:file-exists-p path)
  ;;        (restas.wiki.view:show-page (list :menu menu
  ;;                                          :page (generate-page-html (wiki-parser:parse :dokuwiki
  ;;                                                                                       (read-gzip-file-into-string path)))))
  ;;        (restas.wiki.view:archive-not-found (list :menu menu))))
  ;;  (format nil "Архивная версия - ~A" page)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route show-wiki-page-in-pdf (":(page)/pdf"
                                     :content-type "application/pdf")
  (flexi-streams:with-output-to-sequence (out)
    (let ((out* (flexi-streams:make-flexi-stream out)))
      (pdf-render-wiki-page (wiki-parser:parse :dokuwiki
                                               (wiki-page-pathname page))
                            out*))
    out))


(defparameter *resource-dir*
  (merge-pathnames "resource/"
                   (asdf:component-pathname (asdf:find-system '#:restas-wiki))))

(define-route css ("css/:file")
  (merge-pathnames (format nil "css/~A" file)
                   *resource-dir*))
