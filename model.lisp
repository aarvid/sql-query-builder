(in-package :sql-query-builder)

(defmodel (table-info)
  table-schema
  table-name
  table-type)

(retrieve-one
 (select (:table_schema :table_name :table_type)
   (from :information_schema.tables)
   (where (:= :table_name "project")) ))
