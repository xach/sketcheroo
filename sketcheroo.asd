;;;; v3cto.asd

(asdf:defsystem #:sketcheroo
  :description "2d graphics and layout API."
  :author "Zach Beane <xach@xach.com>"
  :license "BSD"
  :depends-on (#:vecto
               #:cl-pdf)
  :serial t
  :components ((:file "package")
               (:file "primitives")))

