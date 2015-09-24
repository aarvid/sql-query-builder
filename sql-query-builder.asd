;;;; sxql-builder.asd

(asdf:defsystem #:sql-query-builder
  :description "Builds queries for sxql"
  :author "andy peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:sxql
               #:datafly)
  :serial t
  :components ((:file "package")
               (:file "sql-query-builder")))



