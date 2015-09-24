
This project is pre-alpha quality.

For development use only.

The object of this project is tool to generate sql queries to be used in your common lisp code. Currently it targets SxQL and in fact uses the SxQL, datafly, cl-dbi stack of
fukamachi.  

This code is not for production use.  That is do not use it to programatically generate queries and execute them.  The objective of this code is to simply avoid typing.

It should work with an ansi compliant database but currently is only tested on postgresql.

As it uses datafly, you will need to have datafly:*connection* assigned.
If you are using caveman, you can uses (with-connection (db) ...).
Or you can use the function datafly:connect-toplevel.

See documentation strings.

Also read demo.lisp for usage.

