;;;; mnas-clock.asd

(defsystem "mnas-clock"
  :description "Clock Toy"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "MIT"
  :version "0.0.2"
  :depends-on (:alexandria :sdl2kit :defpackage-plus :glkit :mathkit :vlisp)
  :serial t
  :components ((:file "mnas-clock")))
