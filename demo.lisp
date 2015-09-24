;; run these s-expressions one at a time in the repl
;; => shows expected return values
;; print output: shows expected printed output

(in-package :sql-query-builder)
;; => #<PACKAGE "SQL-QUERY-BUILDER">

(defpackage :sql-query-builder-user
  (:use #:cl #:alexandria #:datafly #:sxql :sql-query-builder)
  (:nicknames :sqb-user))
;; => #<PACKAGE "SQL-QUERY-BUILDER-USER">

(in-package :sqb-user)
;; => #<PACKAGE "SQL-QUERY-BUILDER-USER">

;; user needs create privileges for this demo but not to use sql-builder.
(connect-toplevel :postgres :database-name "mydb"
                            :username "myuser" :password "mypassword")
;; => #<DBD.POSTGRES:<DBD-POSTGRES-CONNECTION> {10093D1B63}>


;; use datafly and SxQL to create a table to demonstrate sql-query-builder
(execute
 (create-table :my_table_no_pk
     ((first_integer :type 'integer)
      (second_integer :type 'integer))))
;; => ; No value

;; uses sql-query-builder:select to create a SxQL
(build-select :my_table_no_pk)
;; print output:
;; (select (:first_integer :second_integer)
;;   (from :my_table_no_pk))
;; =>
;; (SELECT (:FIRST_INTEGER :SECOND_INTEGER)
;;   (FROM :MY_TABLE_NO_PK))

(execute
 (create-table :my_table_one_pk
     ((id :type 'integer :primary-key t)
      (id-value :type 'integer))))
;; => ; No value

*allow-dash-for-underscore*
;; => t

;; use dash as separator (input only, never output, sxql does not treat this.)
(build-select :my-table-one-pk)
;; print output:
;; (select (:id :id-value)
;;   (from :my-table-one-pk)
;;   (where (:= :id id)))
;; =>
;; (SELECT (:ID :ID-VALUE)
;;   (FROM :MY_CHILD_TABLE_TWO_PK)
;;   (WHERE (:= :ID ID)))

(execute
 (create-table :my_child_table_two_pk
     ((id :type 'integer)
      (id2 :type 'integer)
      (id2-value :type 'integer))
   (primary-key '(:id :id2))
   (foreign-key '(:id) :references '(:my_table_one_pk :id))))
;; => ; No value

;; use string for table name, specify schema, no pretty output.
(build-select "my_child_table_two_pk" :schema :public :output-stream nil)
;; print output: none
;; =>
;; (SELECT (:ID :ID2 :ID2-VALUE)
;;   (FROM :MY_CHILD_TABLE_TWO_PK)
;;   (WHERE (:AND (:= :ID ID) (:= :ID2 ID2))))


(build-update :my_child_table_two_pk )
;; print output:
;; (update :my_child_table_two_pk
;;   (set= :id id :id2 id2 :id2-value id2-value)
;;   (where (:and (:= :id id) (:= :id2 id2))))
;; =>
;; (UPDATE :MY_CHILD_TABLE_TWO_PK
;;   (SET= :ID ID :ID2 ID2 :ID2-VALUE ID2-VALUE)
;;   (WHERE (:AND (:= :ID ID) (:= :ID2 ID2))))


(build-insert :my_child_table_two_pk)
;; print output:
;; (insert-into :my_child_table_two_pk
;;   (set= :id id :id2 id2 :id2-value id2-value))
;; =>
;; (INSERT-INTO :MY_CHILD_TABLE_TWO_PK
;;   (SET= :ID ID :ID2 ID2 :ID2-VALUE ID2-VALUE))

(build-delete :my_child_table_two_pk )
;; print output:
;; (delete-from :my_child_table_two_pk
;;   (where (:and (:= :id id) (:= :id2 id2))))
;; =>
;; (DELETE-FROM :MY_CHILD_TABLE_TWO_PK
;;   (WHERE (:AND (:= :ID ID) (:= :ID2 ID2))))


(build-delete :my_table_no_pk )
;; print output:
;; (delete-from :my_table_no_pk
;;   (where
;;    (:and (:= :first_integer first-integer)
;;     (:= :second_integer second-integer))))
;; =>
;; (DELETE-FROM :MY_TABLE_NO_PK
;;   (WHERE
;;    (:AND (:= :FIRST_INTEGER FIRST-INTEGER)
;;     (:= :SECOND_INTEGER SECOND-INTEGER))))
