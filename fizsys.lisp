;;; -*- Syntax: Zetalisp; Base: 10 -*-
(defsystem fiziks
  (:name "LFIZIKS -- La Grange Physics minilab")
  (:pathname-default "lfiziks:lfiziks;")
  (:package "user")

  (:module collide "collide")
  (:module delay "delay")
  (:module draw "draw")
  (:module extop "lfiziks:tools;extop")
  (:module fiziks "dfiziks")
  (:module init ("fizinit" "vec"
		 "lfiziks:tools;named"
		 "lfiziks:tools;pop-edit2"
		 "lfiziks:tools;fasd"
		 "delay"))
  (:module pane "lfiziks:tools;pane" :package "tv")
  (:module sim "dsim")
  (:module top "top")
  (:module trail "trail")

  (:compile-load init)
  (:compile-load draw)
  (:compile-load-init  sim (init) (:fasload init draw) (:fasload init draw))
  (:compile-load pane)
  (:compile-load extop (:fasload pane))
  (:compile-load-init trail (init) (:fasload init) (:fasload init))
  (:compile-load top (:fasload sim draw pane extop) (:fasload sim draw pane extop))
  (:compile-load-init fiziks (init) (:fasload init draw sim) (:fasload init draw sim))
  (:compile-load-init collide (init) (:fasload init sim draw fiziks)
   (:fasload init sim draw fiziks))
  )
