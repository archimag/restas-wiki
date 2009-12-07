;;;; preferences.lisp

(in-package #:restas.wiki)

;;;; wiki data dir

(defvar *wiki-dir* nil)

(defun wiki-page-pathname (page)
  (merge-pathnames (format nil "pages/~A" (hunchentoot:url-encode  page))
                   *wiki-dir*))

(defun wiki-page-changes-pathname (page)
  (merge-pathnames (format nil "changes/~A.changes" (hunchentoot:url-encode  page))
                   *wiki-dir*))

(defun wiki-page-archive-pathname (page time)
  (merge-pathnames (format nil "archive/~A.~A.gz" (hunchentoot:url-encode page) time)
                   *wiki-dir*))

;;;; wiki user

(defvar *wiki-user-function* nil)

(defun wiki-user ()
  (if *wiki-user-function*
      (funcall *wiki-user-function*)))


;;;; finalize page

(defvar *finalize-page* #'restas.wiki.view:standalone-frame)

(defun finalize-page (content &optional title)
  (funcall *finalize-page*
           (list :content content
                 :title title
                 :css (list (restas:genurl 'css :file "wiki.css")))))

;;;; generate-page-html

(defun default-generate-page-html (wikipage)
  (xtree:with-object (content (xfactory:with-element-factory ((E))
                                (render-wiki-page wikipage)))
    (xtree:serialize content :to-string)))

(defvar *generate-page-html* #'default-generate-page-html)

(defun generate-page-html (wikipage)
  (funcall *generate-page-html* wikipage))