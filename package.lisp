;;;; package.lisp
(in-package :cl-user)

(defpackage #:sql-query-builder
  (:use #:cl #:alexandria #:datafly #:sxql )
  (:nicknames :sqb)
  (:export :build-select :build-update :build-insert :build-delete
           :db-tables :db-primary-key
           :db-primary-key-columns 
           :db-table-columns :keyword-upcase :ensure-identifier
           :*allow-dash-for-underscore*
           :*schema*))

