;;;; wiki.lisp
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:restas.wiki)

(restas:define-route main-wiki-page ("")
  (list* :title *index-page-title*
         (show-wiki-page :page "index")))

(restas:define-route show-wiki-page (":(page)")
  (list :title page
        :content (storage-find-page *storage* page)
        :menu-links (list :edit-href (restas:genurl 'edit-wiki-page
                                                    :page page)
                          :history-href (restas:genurl 'history-wiki-page
                                                       :page page))))

(restas:define-route edit-wiki-page ("edit/:(page)" 
                                     :requirement #'wiki-user)
  (list :title (format nil "Edit \"~A\"" page)
        :content (storage-find-page *storage* page)))

(restas:define-route edit-wiki-page/preview ("edit/:page"
                                             :method :post
                                             :requirement (lambda () (wsal:post-parameter "preview")))
  (list :title page
        :content (wsal:post-parameter "content")))

(restas:define-route edit-wiki-page/cancel ("edit/:page"
                                            :method :post
                                            :requirement (lambda () (wsal:post-parameter "cancel")))
  (restas:redirect 'show-wiki-page
                   :page page))

(restas:define-route edit-wiki-page/save ("edit/:page"
                                          :method :post
                                          :requirement (lambda () (wsal:post-parameter "save")))
  (storage-save-page *storage*
                     page
                     (wsal:post-parameter "content")
                     (wiki-user))
  (restas:redirect 'show-wiki-page
                   :page page))

(restas:define-route history-wiki-page ("history/:(page)"
                                        :requirement #'wiki-user)
  (list :title (format nil "History of page \"~A\"" page)
        :history (iter (for item in (storage-page-history *storage* page))
                       (collect (list* :href (restas:genurl 'show-archive-wiki-page
                                                            :page page
                                                            :version (getf item :date))
                                       item)))
        :menu-links (list :view-href (restas:genurl 'show-wiki-page
                                                    :page page))))

(restas:define-route show-archive-wiki-page ("history/:(page)/:(version)")
  (list :title (format nil "Archive version of ~A: ~A" page version)
        :content (storage-page-version *storage* page version)
        :menu-links (list :current-version-href (restas:genurl 'show-wiki-page
                                                               :page page)
                          :history-href (restas:genurl 'history-wiki-page
                                                       :page page))))
  