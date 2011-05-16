;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2008-2009 Arturs Grebstelis
;;;;

(in-package #:asdf)

; please shut up dear ASDF
(asdf:oos 'asdf:load-op 'cffi :verbose nil)
(asdf:oos 'asdf:load-op 'cl-glu :verbose nil)
(asdf:oos 'asdf:load-op 'cl-opengl :verbose nil)

(defsystem abop
  :components ((:module :abop
			:pathname "."
			:components
			((:file "common")
			 (:file "geometry"	:depends-on ("common"))
			 (:file "draw"		:depends-on ("geometry"))
			 (:file "opengl"	:depends-on ("draw"))
			 (:file "wavefront"	:depends-on ("opengl"))
			 (:file "povray"	:depends-on ("wavefront"))
			 (:file "collada"	:depends-on ("povray"))
			 (:file "rib"		:depends-on ("collada"))
			 (:file "json"		:depends-on ("rib"))
			 (:file "turtle"	:depends-on ("json"))
			 (:file "system"	:depends-on ("turtle"))
			 (:file "l-system"	:depends-on ("system"))
			 (:file "compile"	:depends-on ("l-system"))
			 (:file "examples"	:depends-on ("compile"))))))
