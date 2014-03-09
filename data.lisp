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
Heheheheeee... You came into my humble home, bold adventurer? You search for treasure? You hope to get out alive? No no no no... My minions are waiting for you. You'll die surely. Come, come! Find me, and I'll give you a present... A present of never ending pain and suffer. Heheheheeee... 
[Press SPACE to continue]")

(defstruct basicdata
  char typeid
  name
  font
  desc)

(defstruct (monsterdata (:include basicdata)))

;;; record structure: (character typeid name)
(defconstant +MONSTERSDATA+ 
  '((:char "r" :typeid "rr" :name "rat" :font 'sans :desc "") 
    (:char "r" :typeid "rm" :name "mole" :font 'sans :desc "")
    (:char "s" :typeid "sp" :name "spider" :font 'sans :desc "") 
    (:char "s" :typeid "sc" :name "centipede" :font 'sans :desc "") 
    (:char "s" :typeid "ss" :name "scorpion" :font 'sans :desc "")
    (:char "w" :typeid "w" :name "worm" :font 'sans :desc "")
    (:char "B" :typeid "B" :name "bat" :font 'sans :desc "")
    (:char "k" :typeid "k" :name "kobold" :font 'sans :desc "")
    (:char "o" :typeid "o" :name "orc" :font 'sans :desc "")
    (:char "h" :typeid "h" :name "humanoid" :font 'sans :desc "")
    (:char "G" :typeid "G" :name "gnome" :font 'sans :desc "")
    (:char "M" :typeid "M" :name "mummy" :font 'sans :desc "")
    (:char "O" :typeid "O" :name "ogre" :font 'sans :desc "")
    (:char "T" :typeid "T" :name "troll" :font 'sans :desc "")
    (:char "S" :typeid "S" :name "snake" :font 'sans :desc "")
    (:char "Z" :typeid "Z" :name "zombie" :font 'sans :desc "")
    (:char "@" :typeid "@*" :name "human" :font 'sans :desc "")
    (:char "&" :typeid "&" :name "daemon" :font 'sans-bold :desc "")))

(defconstant +ALIGNMENTSDATA+ '((:id 'lawful :desc "") 
				 (:id 'neutral :desc "") 
				 (:id 'chaotic :desc "")))

(defstruct (itemdata (:include basicdata)))

(defconstant +ITEMSDATA+ 
  '((:char "\"" :typeid "\"" :name "amulet" :font 'sans :desc "") 
    (:char ")" :typeid ")" :name "weapon" :font 'sans :desc "")
    (:char "[" :typeid "[" :name "armor" :font 'sans :desc "")
    (:char "!" :typeid "!" :name "potion" :font 'sans :desc "")
    (:char "?" :typeid "?" :name "scroll" :font 'sans :desc "")
    (:char "/" :typeid "/" :name "wand" :font 'sans :desc "")
    (:char "=" :typeid "=" :name "ring" :font 'sans :desc "")
    (:char "*" :typeid "*" :name "gem" :font 'sans :desc "")
    (:char "(" :typeid "(" :name "statue" :font 'sans :desc "")
    (:char "$" :typeid "$" :name "coins" :font 'sans :desc "")
    (:char "%" :typeid "%" :name "comestible" :font 'sans :desc "")))

(defstruct (dungeonfeaturedata (:include basicdata)))

(defconstant +DUNGEONFEATURESDATA+ 
  '((:char ">" :typeid ">" :name "ladder up" :font 'sans :desc "") 
    (:char "<" :typeid "<" :name "ladder down" :font 'sans :desc "")
    (:char "_" :typeid "_" :name "altar" :font 'sans :desc "")
    (:char "{" :typeid "{" :name "fountain" :font 'sans :desc "")
    (:char "^" :typeid "^" :name "trap" :font 'sans :desc "")
    (:char "+" :typeid "+" :name "closed door" :font 'sans :desc "")
    (:char "-" :typeid "-" :name "open door" :font 'sans :desc "")
    (:char "|" :typeid "|" :name "open door" :font 'sans :desc "")
    (:char "." :typeid "." :name "floor" :font 'sans :desc "")
    (:char "#" :typeid "#" :name "wall" :font 'sans :desc "")
    (:char "\\" :typeid "\\" :name "throne" :font 'sans :desc "")))

(defstruct (playerdata (:include basicdata)))

(defconstant +PLAYERDATA+ 
  '(:char "@" :typeid "@" :name "player" :font 'sans :desc ""))

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

;-------------------------------------------------------------------------------|
(defconstant +TESTLEVEL+
"                          ###############
##########                #.............#                          ############
#........#              ###.............#                    #######.....)....#
#..>.....################.+.......\".....#                    #.....+..........#
#.)...[..+...............##.%..!....$...#                    #.#####.......$..#
#........###.############ #.............#                    #.#   ############
########## #.#            ############+##                    #.#
           #.#                       #.#                     #.#
     #######.#              ##########.#   ###################+###
     #....##.#              #..........# ###.....................#
     #.{..+..#              #.########## #.+...=..........._.....#
     #....####              #.#          #.#.....................#
     ######                 #.############.#................\\.*..#
                            #..............#............../...<..#
                            ###+############.....................#
                              #.#          #######################
                              #.#
                              #.#
                              #.#
       ##############         #.#
       #.$..........###########.#
       #............+...........#
       #............#############
       #......%.....#
       ##############")
