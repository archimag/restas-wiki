;;;; restas-wiki.asd
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defsystem restas-wiki
  :depends-on (#:restas #:wiki-parser #:cl-libxml2 #:xfactory #:colorize #:local-time #:zip #:cl-typesetting #:closure-template)
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:module "dokuwiki"
                      :components
                      ((:file "render-html")
                       (:file "render-pdf"))
                      :depends-on ("packages"))
             (:file "preferences" :depends-on ("dokuwiki"))
             (:file "storage" :depends-on ("packages"))
             (:file "drawer" :depends-on ("dokuwiki"))
             (:file "wiki" :depends-on ("preferences" "storage" "drawer"))))))
    