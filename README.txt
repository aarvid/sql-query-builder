
This project is alpha quality.

For development use only.

This system is a tool to generate sql queries to be used in your common lisp code. Currently it targets SxQL and in fact uses the SxQL, datafly, cl-dbi stack of fukamachi.  

This code is not for production use.  That is, do not use it to programatically generate queries and execute them.  The objective of this library is to simply avoid typing.

It should work with any ansi compliant database that has information_schema but has only been tested on postgresql.

As it uses datafly, you will need to have datafly:*connection* assigned.

If you are using caveman, you can uses (with-connection (db) ...).

Or you can use the function datafly:connect-toplevel for a persistent connection.

Output works best when in a package that uses SxQL so SxQL symbols are printed without package name (i.e. select versus sxql:select)

See documentation strings.

Also read demo.lisp for usage.

