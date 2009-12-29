;;;; packages.lisp
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(restas:define-plugin #:restas.wiki
  (:use #:cl #:iter #:restas.optional)
  (:export #:*wiki-dir*
           #:*wiki-user-function*
           #:*finalize-page*
           #:*generate-page-html*
           #:*index-page-title*))

(in-package #:restas.wiki)

(defparameter *restas-wiki-dir*
  (asdf:component-pathname (asdf:find-system '#:restas-wiki)))

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/wiki.tmpl"
                                                    *restas-wiki-dir*))