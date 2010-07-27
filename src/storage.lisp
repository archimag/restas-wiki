;;;; storage.lisp
;;;;
;;;; This file is part of the restas-wiki library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:restas.wiki)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; storage generic interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric storage-find-page (storage title)
  (:documentation "Find wiki page in storage"))

(defgeneric storage-save-page (storage title content author &optional comment)
  (:documentation "Save wiki page in storage"))

(defgeneric storage-page-history (storage title)
  (:documentation "Return page history"))

(defgeneric storage-page-revision (storage title revision)
  (:documentation "Find wiki page revision in storage"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; file storage 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass file-storage ()
  ((dir :initarg :dir :reader file-storage-dir)))

;;;; aux

(defun encode-page-title (title)
  (closure-template:encode-uri title))

(defun file-storage-page-pathname (storage title)
  (merge-pathnames (format nil "pages/~A" (encode-page-title title))
                   (file-storage-dir storage)))

(defun file-storage-changes-pathname (storage title)
  (merge-pathnames (format nil "changes/~A.changes" (encode-page-title title))
                   (file-storage-dir storage)))

(defun file-storage-archive-pathname (storage page time)
  (merge-pathnames (format nil "archive/~A.~A.gz" (encode-page-title page) time)
                   (file-storage-dir storage)))

;;;; implementation

(defmethod storage-find-page ((storage file-storage) title)
  (fad:file-exists-p (file-storage-page-pathname storage title)))

(defmethod storage-save-page ((storage file-storage) title content author &optional comment)
  (let* ((time (get-universal-time))
         (page-path (ensure-directories-exist (file-storage-page-pathname storage title)))
         (changes-path (ensure-directories-exist (file-storage-changes-pathname storage title)))
         (archive-path (ensure-directories-exist (file-storage-archive-pathname storage title time)))
         (changes (nconc (if (fad:file-exists-p changes-path)
                             (with-open-file (in changes-path)
                               (with-standard-io-syntax
                                 (read in))))
                         (list (list time
                                     author
                                     (if (fad:file-exists-p page-path)
                                         :edit
                                         :create)
                                     title
                                     comment)))))
    (with-open-file (out changes-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (with-standard-io-syntax
        (print changes
               out)))
    (write-string-into-gzip-file content archive-path)
    (alexandria:write-string-into-file content
                                       page-path
                                       :if-exists :supersede
                                       :if-does-not-exist :create)))

(defmethod storage-page-history ((storage file-storage) title)
  (let ((path (file-storage-changes-pathname storage title)))
    (when (fad:file-exists-p path)
      (iter (for item in (nreverse (with-open-file (in path)
                                     (with-standard-io-syntax
                                       (read in)))))
            (collect (list :date (first item)
                           :name (fourth item)
                           :author (second item)))))))
            
(defmethod storage-page-revision ((storage file-storage) title revision)
  (read-gzip-file-into-string (file-storage-archive-pathname storage title revision)))
