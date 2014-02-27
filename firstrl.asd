;;;; firstrl.asd

(in-package :cl-user)

(defpackage #:firstrl-asd
  (:use :cl :asdf))

(in-package #:firstrl-asd)

(asdf:defsystem #:firstrl
;  :serial t
  :description "firstrl is my first try on a roguelike"
  :author "Kalman Kiss <kiskami@freemail.hu>"
  :version "0.0.1"
  :license "GPL2"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-image
	       #:lispbuilder-sdl-ttf)
  :components ((:file "package")
	       (:file "common" :depends-on ("package"))
	       (:file "utils" :depends-on ("package" "common"))
	       (:file "data" :depends-on ("package"))
	       (:file "console" :depends-on ("package" "common" "utils"))
               (:file "firstrl" :depends-on ("package" "common" "utils" "console" "data"))))

