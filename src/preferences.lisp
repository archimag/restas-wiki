;;;; preferences.lisp
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:restas.wiki)

;;;; title index page

;;(defvar *index-page-title* "Index")

;;;; wiki data dir

;; (defvar *wiki-dir* nil)

;; (defun wiki-page-pathname (page)
;;   (merge-pathnames (format nil "pages/~A" (closure-template:encode-uri page))
;;                    *wiki-dir*))

;; (defun wiki-page-changes-pathname (page)
;;   (merge-pathnames (format nil "changes/~A.changes" (hunchentoot:url-encode  page))
;;                    *wiki-dir*))

;; (defun wiki-page-archive-pathname (page time)
;;   (merge-pathnames (format nil "archive/~A.~A.gz" (hunchentoot:url-encode page) time)
;;                    *wiki-dir*))

;;;; wiki user



;;;; finalize page

;; (defvar *finalize-page* #'restas.wiki.view:standalone-frame)

;; (defun finalize-page (content &optional title)
;;   (funcall *finalize-page*
;;            (list :content content
;;                  :title title
;;                  :css (list (restas:genurl 'css :file "wiki.css")))))

;;;; generate-page-html

;; (defun default-generate-page-html (wikipage)
;;   (xtree:with-object (content (xfactory:with-element-factory ((E))
;;                                 (render-wiki-page wikipage)))
;;     (xtree:serialize content :to-string)))

;; (defvar *generate-page-html* #'default-generate-page-html)

;; (defun generate-page-html (wikipage)
;;   (funcall *generate-page-html* wikipage))