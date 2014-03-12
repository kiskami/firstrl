;;;; firstrl.asd
;; Copyright (c) 2014 Kalman Kiss, Zalaegerszeg Hungary
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;; Author and contact:
;; Kalman Kiss <kiskami@freemail.hu>
;; 8900 Zalaegerszeg, Hungary
;; Kakukkfu u. 4.

(in-package :cl-user)

(defpackage #:firstrl-asd
  (:use :cl :asdf))

(in-package #:firstrl-asd)

(asdf:defsystem #:firstrl
  :description "firstrl is my first try on a roguelike"
  :author "Kalman Kiss <kiskami@freemail.hu>"
  :version "0.0.1"
  :license "GPL2"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-image
	       #:lispbuilder-sdl-ttf)
  :components ((:file "package")
	       (:file "common" :depends-on ("package"))
	       (:file "data" :depends-on ("package"))
	       (:file "utils" :depends-on ("package" "data" "common"))
	       (:file "dungen" :depends-on ("package" "data" "common" "utils"))
	       (:file "console" :depends-on ("package" "common" "utils"))
               (:file "firstrl" :depends-on ("package" "common" "data" "utils"
						       "console" "dungen"))))

