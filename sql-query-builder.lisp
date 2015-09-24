;;;; sql-query-builder.lisp

(in-package #:sql-query-builder)

(defvar *allow-dash-for-underscore* t
  "means :my-table-name is equivalent to :my_table_name. to ease code-completion")

(defun keyword-upcase (name)
  "make an upper case keyword from a string or another keyword"
  (when (keywordp name)
    (setf name (symbol-name name)))
  (make-keyword (string-upcase name)))

(defun ensure-identifier (id)
  "ensure id is a sql-identifier string"
  (if (keywordp id)
      (string-downcase
       (funcall
        (if *allow-dash-for-underscore*
            (curry #'substitute #\_ #\-)
            #'identity)
        (symbol-name id)))
      id))

(defun ensure-sxql-keyword (kw)
  "ensure that kw is a sxql keyword"
  (keyword-upcase
   (if (keywordp kw)
       (funcall (if *allow-dash-for-underscore*
                    (curry #'substitute #\_ #\-)
                    #'identity)
                (symbol-name kw))
       kw)))

(defun ensure-variable-symbol (kw)
  "ensure that symbol kw is a local symbol with dashes instead of underscore.
kw: symbol, keyword or string
:my_column_name -> 'my-column-name "
  (intern (substitute #\- #\_ (if (symbolp kw) (symbol-name kw) (string-upcase kw)))))

(defvar *schema* (ensure-identifier :public)
  "default database schema for build functions")

(defun db-tables (&key (schema *schema*) (transform #'identity))
  "returns a list of table names of the schema.
schema: string or keyword representing schema
transform: function of one string variable to transform table names.
           use #'keyword-upcase for keyword names"
  (mapcar transform
          (retrieve-all-values
           (select :table_name
             (from :information_schema.tables)
             (where (:= :table_schema (ensure-identifier schema)))))))

(defun db-table-columns (schema table &key (transform #'identity))
  "returns a list of column names of the table.
schema: string or keyword representing schema
table: string or keyword representing the table
transform: function of one string variable to transform column names.
           use #'keyword-upcase for keyword names"
  (mapcar transform
          (retrieve-all-values
           (select :column_name
             (from :information_schema.columns)
             (where (:and (:= :table_schema (ensure-identifier schema))
                          (:= :table_name (ensure-identifier table))))
             (order-by :ordinal_position)))))

(defun db-primary-key (schema table &key (transform #'identity))
  "returns two values constaint-schema e constraint-name of the primary key.
schema: string or keyword representing schema
table: string or keyword representing the table
transform: function of one string variable to transform the results.
           use #'keyword-upcase for keyword names"
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
  "returns a list of column names of the primary key of the table.
schema: string or keyword representing schema
table: string or keyword representing the table
transform: function of one string variable to transform column names.
           use #'keyword-upcase for keyword names"
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

(defun schema-code-completion (&key (schema *schema*)
                                    (columns-p t)
                                    (schema.table-p nil)
                                    (table-dashes-p *allow-dash-for-underscore*))
    "internalizes the table-names and possibly column-names of a schema to assist
code completion. returns no value. 
schema: string or keyword representing schema
columns-p: boolean, makes keywords for all column names of each table.
schema.table-p: boolean, for each table make an additional keyword for schema.
table-dashes-p: boolean, for each table make an additional keyword with dashes instead of underscore."
  (let ((sch (ensure-identifier schema)))
    (dolist (tb (db-tables :schema schema))
      (keyword-upcase tb)
      (when schema.table-p
        (keyword-upcase (concatenate 'string sch "." tb)))
      (when table-dashes-p
        (keyword-upcase (substitute #\- #\_ tb)))
      (when columns-p
        (db-table-columns schema tb :transform #'keyword-upcase))))
  (values))




(defun build-pk-search-condition (cols)
  "returns a s-expression of the clauses of a search condition (where) by primary key.
cols: list of the primary key columns as sxql keywords"
  (let ((clauses (mapcar (lambda (c)
                           (list := c (ensure-variable-symbol c )))
                         cols)))
    (if (sequence-of-length-p clauses 1)
        (car clauses)
        (cons :and clauses))))

(defun build-set=-clause (cols)
  "returns a s-expression of the set= for inserts and updates
cols: list of the primary key columns as sxql keywords"
  (cons 'set= (mapcan (lambda (c)
                        (list c (ensure-variable-symbol c)))
                      cols)))


(defun maybe-output-fn (output-stream)
  (if output-stream
      (lambda (x) (let ((*print-case* :downcase))
                    (pprint x output-stream)
                    x))
      #'identity))


(defun build-select (table &key (schema *schema*) (output-stream *standard-output*))
  "returns SxQL select s-expression
schema: string or keyword representing schema
table: string or keyword representing the table
output-stream: pretty print in lower case to this stream. nil means no printing. "
  (when-let ((cols (db-table-columns schema table :transform #'keyword-upcase)))
    (let ((pk-cols (db-primary-key-columns schema table :transform #'keyword-upcase)))
     (funcall (maybe-output-fn output-stream)
              (append (list 'select
                            cols
                            (list 'from (ensure-sxql-keyword table)))
                      (when pk-cols
                        (list (list 'where (build-pk-search-condition pk-cols)))))))))


(defun build-update (table &key (schema *schema*) (output-stream *standard-output*))
  "returns SxQL update s-expression
schema: string or keyword representing schema
table: string or keyword representing the table
output-stream: pretty print in lower case to this stream. nil means no printing. "
  (when-let ((cols (db-table-columns schema table :transform #'keyword-upcase)))
    (let ((pk-cols (db-primary-key-columns schema table :transform #'keyword-upcase)))
      (funcall (maybe-output-fn output-stream)
               (list 'update
                     (ensure-sxql-keyword table)
                     (build-set=-clause cols)
                     (list 'where (build-pk-search-condition (or pk-cols cols))))))))

(defun build-insert (table &key (schema *schema*) (output-stream *standard-output*))
    "returns SxQL insert-into s-expression
schema: string or keyword representing schema
table: string or keyword representing the table
output-stream: pretty print in lower case to this stream. nil means no printing. "
  (when-let ((cols (db-table-columns schema table :transform #'keyword-upcase)))
    (let ((pk-cols (db-primary-key-columns schema table :transform #'keyword-upcase)))
     (funcall (maybe-output-fn output-stream)
              (list 'insert-into
                           (ensure-sxql-keyword table)
                           (build-set=-clause cols))))))

(defun build-delete (table &key (schema *schema*) (output-stream *standard-output*))
  "returns SxQL delete-from s-expression
schema: string or keyword representing schema
table: string or keyword representing the table
output-stream: pretty print in lower case to this stream. nil means no printing. "
  (when-let ((cols (db-table-columns schema table :transform #'keyword-upcase)))
    (let ((pk-cols (db-primary-key-columns schema table :transform #'keyword-upcase)))
      (funcall (maybe-output-fn output-stream)
               (list 'delete-from
                     (ensure-sxql-keyword table)
                     (list 'where (build-pk-search-condition (or pk-cols cols))))))))





(defun build-create (table &key (schema *schema*) (output-stream *standard-output*))
  "returns SxQL create s-expression
schema: string or keyword representing schema
table: string or keyword representing the table
output-stream: pretty print in lower case to this stream. nil means no printing. "
  (when-let ((cols (db-table-columns schema table :transform #'keyword-upcase)))
    (let ((pk-cols (db-primary-key-columns schema table :transform #'keyword-upcase)))
      (funcall (maybe-output-fn output-stream)
               (list 'create
                     (ensure-sxql-keyword table)
                     (list 'where (build-pk-search-condition (or pk-cols cols))))))))

(defun build-selects (table &key (schema *schema*) (output-stream *standard-output*))
  "returns SxQL select s-expression
schema: string or keyword representing schema
table: string or keyword representing the table or a list of them.
output-stream: pretty print in lower case to this stream. nil means no printing. "
  (let ((tables (ensure-list table)))
    )
  (when-let ((cols (db-table-columns schema table :transform #'keyword-upcase)))
    (let ((pk-cols (db-primary-key-columns schema table :transform #'keyword-upcase)))
     (funcall (maybe-output-fn output-stream)
              (append (list 'select
                            cols
                            (list 'from (ensure-sxql-keyword table)))
                      (when pk-cols
                        (list (list 'where (build-pk-search-condition pk-cols)))))))))
