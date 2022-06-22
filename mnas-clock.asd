;;;; mnas-clock.asd

(defsystem "mnas-clock"
  :description "Clock Toy"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "MIT"
  :version "0.0.3"
  :depends-on (:alexandria :sdl2kit :defpackage-plus :glkit :mathkit :vlisp)
  :serial t
  :components ((:module "src"
                :serial nil
                :components ((:file "mnas-clock")))))

