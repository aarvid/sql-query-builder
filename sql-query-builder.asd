;;;; sql-query-builder.asd

(asdf:defsystem #:sql-query-builder
  :description "Builds queries for sxql for developmental use"
  :author "andy peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:sxql
               #:datafly)
  :serial t
  :components ((:file "package")
               (:file "sql-query-builder")))



