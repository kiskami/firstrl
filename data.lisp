;;; data.lisp

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

(defconstant +DUNGEONFEATURES+ '((">" ">" "ladder up") ("<" "<" "ladder down")
				 ("_" "_" "altar")
				 ("{" "{" "fountain")
				 ("^" "^" "trap")
				 ("+" "+" "closed door")
				 ("-" "-" "open door")
				 ("|" "|" "open door")
				 ("." "." "floor")
				 ("#" "#" "corridor")
				 ("\\" "\\" "throne")))

(defstruct player
  name
  typeid
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
  inventory
  
  aktlevel
  x y
  )

(defstruct level
  name
  parents childs
  monsters
  items
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

(defparameter get-monster-char #'first)
(defparameter get-monster-typeid #'second)
(defparameter get-item-char #'first)
(defparameter get-item-typeid #'second)
(defparameter get-dungeonfeature-char #'first)
(defparameter get-dungeonfeature-typeid #'second)

(defun find-monster-data (id &key (getter get-monster-typeid))
  (find-if #'(lambda (r) (equal id r)) +MONSTERS+ :key getter))

(defun find-item-data (id &key (getter get-item-typeid))
  (find-if #'(lambda (r) (equal id r)) +ITEMS+ :key getter))

(defun find-dungeonfeature-data (id &key (getter get-dungeonfeature-typeid))
  (find-if #'(lambda (r) (equal id r)) +DUNGEONFEATURES+ :key getter))
