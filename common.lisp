;;; common.lisp

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
Source code is at <tba>")

(defconstant +WINDOW-W+ 80 "Game window width in characters (tiles)")
(defconstant +WINDOW-H+ 60 "Game window height in characters (tiles)")
(defconstant +TILESIZE+ 10)

(defparameter +BLACK-COLOR+ sdl:*black*)
(defparameter +WHITE-COLOR+ sdl:*white*)
(defparameter +DEFCOLOR+ +WHITE-COLOR+)

(defvar *RNDSTATE* (make-random-state t))

(defvar *FONTMAP* nil "Fonts used by this game, intialized in console.")

(defstruct player
  name
  gender
  role
  alignment
  deity
  inventory
  armor
  weapons
  tools
  hp)

(defstruct dungeon
  player
  levels

  )
