;;;; package.lisp
(in-package :cl-user)

(defpackage #:sql-query-builder
  (:use #:cl #:alexandria #:datafly #:sxql )
  (:nicknames :sqb)
  (:export :db-tables :db-primary-key
           :db-primary-key-columns :build-select
           :db-table-columns :keyword-upcase :ensure-identifier
           :*allow-dash-for-underscore*
           :*schema*))

