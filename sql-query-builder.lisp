;;;; sxql-builder.lisp

(in-package #:sql-query-builder)

;;; "sxql-builder" goes here. Hacks and glory await!




(defun keyword-upcase (name)
  (when (keywordp name)
    (setf name (symbol-name name)))
  (make-keyword (string-upcase name)))

(defun ensure-identifier (id)
  (if (keywordp id)
      (string-downcase (symbol-name id))
      id))

(defun ensure-keyword (kw)
  (if (keywordp kw)
      kw
      (make-keyword kw)))

(defvar *schema* (ensure-identifier :public))

(defun db-tables (&key (schema *schema*) (transform #'identity))
  (mapcar transform
          (retrieve-all-values
           (select :table_name
             (from :information_schema.tables)
             (where (:= :table_schema (ensure-identifier schema)))))))

(defun db-table-columns (schema table &key (transform #'identity))
  (mapcar transform
          (retrieve-all-values
           (select :column_name
             (from :information_schema.columns)
             (where (:and (:= :table_schema (ensure-identifier schema))
                          (:= :table_name (ensure-identifier table))))
             (order-by :ordinal_position)))))

(defun db-primary-key (schema table &key (transform #'identity))
  (when-let ((pk
              (retrieve-one
               (select (:constraint_schema :constraint_name)
                 (from :information_schema.table_constraints)
                 (where (:and (:= :table_schema (ensure-identifier schema))
                              (:= :table_name (ensure-identifier table))
                              (:= :constraint_type "PRIMARY KEY")))))))
    (values (funcall transform (getf pk :constraint-schema))
            (funcall transform (getf pk :constraint-name)))))

(defun db-primary-key-columns (schema table &key (transform #'identity))
  (multiple-value-bind (c-schema c-name) (db-primary-key schema table)
    (when (and c-schema c-name)
      (mapcar transform
              (retrieve-all-values
               (select :column_name
                 (from :information_schema.key_column_usage)
                 (where (:and (:= :table_schema (ensure-identifier schema))
                              (:= :table_name (ensure-identifier table))
                              (:= :constraint_schema c-schema)
                              (:= :constraint_name c-name)))
                 (order-by :ordinal_position)))))))

(defun ensure-variable-symbol (kw)
  (intern (substitute #\- #\_ (symbol-name kw))))

(defun build-pk-where (cols)
  (let ((clauses (mapcar (lambda (c)
                           (list := c (ensure-variable-symbol c )))
                         cols)))
    (if (sequence-of-length-p clauses 1)
        (car clauses)
        (cons :and clauses))))

(defun build-select (table &key (schema *schema*) (output-stream *standard-output*))
  (when-let* ((cols (db-table-columns schema table :transform #'keyword-upcase))
              (pk-cols (db-primary-key-columns schema table :transform #'keyword-upcase)))
    (funcall (if output-stream
                 (lambda (x) (let ((*print-case* :downcase))
                               (pprint x output-stream)
                               x))
                 #'identity)
             (append (list 'select
                           cols
                           (list 'from (ensure-keyword table)))
                     (when pk-cols
                       (list (list 'where (build-pk-where pk-cols))))))))


;;(connect-toplevel :postgres :database-name "dbtullius" :username "utullius" :password "Ulysses")
