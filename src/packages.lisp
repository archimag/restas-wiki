;;;; packages.lisp

(restas:define-plugin #:restas.wiki
  (:use #:cl #:iter #:restas.optional)
  (:export #:*wiki-dir*
           #:*wiki-user-function*
           #:*finalize-page*
           #:*generate-page-html*))

(in-package #:restas.wiki)

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/wiki.tmpl"
                                                    (asdf:component-pathname (asdf:find-system '#:restas-wiki))))