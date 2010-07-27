;;;; restas-wiki.asd
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defsystem restas-wiki
  :depends-on (#:restas #:local-time #:zip #:closure-template)
  :components
  ((:module "src"
            :components
            ((:file "defmodule")             
             (:file "storage" :depends-on ("defmodule"))
             (:file "drawer" :depends-on ("defmodule"))
             (:file "routes" :depends-on ("storage" "drawer"))))))
    