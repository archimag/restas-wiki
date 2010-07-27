;;;; packages.lisp
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(restas:define-module #:restas.wiki
  (:use #:cl #:iter #:restas.optional)
  (:export #:*wiki-user-function*

           ;; storage
           #:*storage*
           #:file-storage
           #:storage-find-page
           #:storage-save-page
           #:storage-page-history

           ;; drawer
           #:drawer
           #:finalize-page
           #:render-route-data
           #:generate-content-from-markup))

(in-package #:restas.wiki)

(defvar *storage*)

(defparameter *restas-wiki-dir*
  (asdf:component-pathname (asdf:find-system '#:restas-wiki)))

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/wiki.tmpl"
                                                    *restas-wiki-dir*))

(defvar *wiki-user-function* nil)

(defun wiki-user ()
  (if *wiki-user-function*
      (funcall *wiki-user-function*)))
