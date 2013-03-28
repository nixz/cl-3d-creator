cl-3d-creator
=============

* This project works in conjunction with cl-3d and is used to generate
  the automatic parser for reading X3D files in cl-3d.

* The parser is auto-generated from X3D's schema
  definitions. Nevertheless the schema is slightly augmented to fit
  our needs.

* We use cl-xsd to read the specification into lisp objects.

* You may not need this project unless you want to contribute to cl-3d
  or use this project as a template to generate you own automatic
  parser generator for other schema definitions.

In any case here is the usage:

* Checkout XMLisp cl-xsd and cl-3d-creator into
  quicklisp/local-projects directory.

  cd ~/quicklisp/local-projects ;; or wherever you have it
  
  git clone git@github.com:nixz/XMLisp.git
  git clone git@github.com:nixz/cl-xsd.git
  git clone git@github.com:nixz/cl-3d-creator.git

* In common lisp REPL
  CL-USER > (x3d:GENERATE "~/cl-3d/")
