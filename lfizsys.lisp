;;; -*- Syntax: Zetalisp; Base: 10 -*-
(defsystem lfiziks
  (:name "LFIZIKS -- Lagrange Physics minilab")
  (:pathname-default "lfiziks:lfiziks;")
  (:package "user")

  (:module init ("fizinit" "vec"
		 "lfiziks:tools;pop-edit"
		 "histo"
		 "object-with-process"
		 ))

  #+ignore (:module electron ("electron"))

  (:compile-load init)
  #+ignore
  (:compile-load electron (:fasload init) (:fasload :init))) 
