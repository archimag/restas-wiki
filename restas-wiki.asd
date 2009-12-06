;;;; restas-wiki.asd

(defsystem restas-wiki
  :depends-on (#:restas #:wiki-parser #:cl-libxml2 #:xfactory #:colorize #:local-time #:zip #:cl-typesetting)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "render-html" :depends-on ("packages"))
                                     (:file "render-pdf" :depends-on ("packages"))
                                     (:file "preferences" :depends-on ("render-html" "render-pdf"))
                                     (:file "wiki" :depends-on ("preferences" ))))))
    