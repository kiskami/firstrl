;;; common.lisp
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

(in-package #:firstrl)

(defconstant +GAME-LABEL+ "firstrl - my first try on a roguelike
Copyright (c) 2014 Kalman Kiss, Zalaegerszeg Hungary

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Contact: Kalman Kiss <kiskami@freemail.hu>
Source code is at https://code.google.com/p/firstrl/")

(defconstant +NORMAL-FONT-SIZE+ 12)
(defconstant +SMALL-FONT-SIZE+ 10)
(defconstant +BIG-FONT-SIZE+ 16)

(defconstant +TILESIZE+ +NORMAL-FONT-SIZE+)
(defconstant +WINDOW-W+ 80 "Game window width in characters (tiles)")
(defconstant +WINDOW-H+ 40 "Game window height in characters (tiles)")

(defconstant +MSGWIND-X+ 1)
(defconstant +MSGWIND-Y+ 27)
(defconstant +MSGWIND-H+ 4)

(defparameter +BLACK-COLOR+ sdl:*black*)
(defparameter +WHITE-COLOR+ sdl:*white*)
(defparameter +GRAY-COLOR+ (sdl:color :r 170 :g 170 :b 170 :a 1))
(defparameter +DEFCOLOR+ +WHITE-COLOR+)
(defparameter +CORPSECOLOR+ +GRAY-COLOR+)

(defparameter *RNDSTATE* nil)

(defparameter *monsterdata* (make-hash-table :test #'equal))
(defparameter *itemdata* (make-hash-table :test #'equal))
(defparameter *dungeonfeaturedata* (make-hash-table :test #'equal))
(defparameter *leveldata* (make-hash-table :test #'equal))
(defparameter *playerdata* nil)
