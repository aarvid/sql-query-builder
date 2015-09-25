
# SQL Query Builder

This project is alpha quality.

For development use only.

# Overview

This system is a tool to generate sql queries to be used in your common lisp code. Currently it targets SxQL and in fact uses the [SxQL][sxql], [datafly][datafly], [cl-dbi][cl-dbi] stack of fukamachi.  

This code is not for production use.  That is, do not use it to programatically generate queries and execute them.  The objective of this library is to simply avoid typing.

It should work with any ansi compliant database that has information_schema but has only been tested on postgresql.

As it uses datafly, you will need to have datafly:*connection* assigned.

If you are using [caveman][caveman], you can uses (with-connection (db) ...).

Or you can use the function datafly:connect-toplevel for a persistent connection.

Output works best when in a package that uses SxQL so SxQL symbols are printed without package name (i.e. select versus sxql:select)

# Documentation

See documentation in the html directory.

Also read demo.lisp for usage.

# License

Copyright (c) 2015 Andrew Peterson

Licensed under the MIT License.

[sxql]: https://github.com/fukamachi/sxql
[datafly]: https://github.com/fukamachi/datafly
[cl-dbi]: http://8arrow.org/cl-dbi/
[caveman]: http://8arrow.org/caveman/ 

