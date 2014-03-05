;;; data.lisp
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

(defconstant +INTRO-TEXT+ "INTRO
Heheheheeee... You came into my humble home, bold adventurer?
You search for treasure? You hope to get out alive? No no no no... My minions are waiting
for you. You'll die surely. Come, come! Find me, and I'll give you a present... A present
of never ending pain and suffer. Heheheheeee...
[Press SPACE to continue]")

;;; record structure: (character typeid name)
(defconstant +MONSTERS+ '(("r" "rr" "rat") ("r" "rm" "mole")
			  ("s" "sp" "spider") ("s" "sc" "centipede") ("s" "ss" "scorpion")
			  ("w" "w" "worm")
			  ("B" "B" "bat")
			  ("k" "k" "kobold")
			  ("o" "o" "orc")
			  ("h" "h" "humanoid")
			  ("G" "G" "gnome")
			  ("M" "M" "mummy")
			  ("O" "O" "ogre")
			  ("T" "T" "troll")
			  ("S" "S" "snake")
			  ("Z" "Z" "zombie")
			  ("@" "@" "human")
			  ("&" "&" "daemon")))

(defconstant +ALIGNMENTS+ '(('lawful) ('neutral) ('chaotic)))

(defconstant +ITEMS+ '(("\"" "\"" "amulet") 
		       (")" ")" "weapon")
		       ("[" "]" "armor")
		       ("!" "!" "potion")
		       ("?" "?" "scroll")
		       ("/" "/" "wand")
		       ("=" "=" "ring")
		       ("*" "*" "gem")
		       ("(" "(" "statue")
		       ("$" "$" "coins")
		       ("%" "%" "comestible")))

(defconstant +DUNGEONFEATURES+ '((">" ">" "ladder up") 
				 ("<" "<" "ladder down")
				 ("_" "_" "altar")
				 ("{" "{" "fountain")
				 ("^" "^" "trap")
				 ("+" "+" "closed door")
				 ("-" "-" "open door")
				 ("|" "|" "open door")
				 ("." "." "floor")
				 ("#" "#" "corridor")
				 ("\\" "\\" "throne")))

(defstruct object
  name
  typeid
  aktlevel
  x y
  state
  thinkfunc)

(defstruct (lifeform (:include object))
  gender
  role
  deity
  armor
  weapons
  tools
  magic
  hp maxhp
  power maxpower
  alignment
  xp
  str maxstr
  dex maxdex
  con maxcon
  int maxint
  wis maxwis
  cha maxcha 				; charisma
  purse
  inventory)

(defstruct level
  name
  parents childs
  monsters
  items
  features
  map
  )

(defstruct dungeon
  player
  levels
  turns)

(defconstant +TESTLEVEL+
"
                  .............
........          .............
..>.....     #####+.......\"....
.)...].+######    .%..!....$...
........   #      ...........+.
           #                 #
           #                 #
           #         # # # # #
      .... #         #       #         .....................
      .{.+##         #       ######### +...=..........._....
      ....           #                 .....................
                                       ................\\.*.
                                       ............../...<..
                                       .....................

")
