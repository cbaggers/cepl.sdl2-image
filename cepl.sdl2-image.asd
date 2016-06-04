;;;; cepl.sdl2-image.asd

(asdf:defsystem #:cepl.sdl2-image
  :description "Some helper methods for using sdl2-image to load images to CEPL types"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:cepl #:sdl2 #:sdl2-image)
  :components ((:file "package")
               (:file "load-image")))
