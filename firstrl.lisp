;;;; firstrl.lisp
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

(defun run (&optional (w +WINDOW-W+) (h +WINDOW-H+))
  (format t "~A~%" +GAME-LABEL+)
  (with-init-console ()
    (let ((console (create-window w h "firstrl"))
	  (should-exit nil)
	  (gamestate nil)
	  (dungeon nil))
      (loop until should-exit do
	   (setf gamestate 
		 (case gamestate
		   ('chargen
		    (format t "chargen...~%")
		    (let ((player (do-chargen console)))
		     (setf dungeon (make-dungeon
				    :player player
				    :levels (gen-levels player)
				    :turns 0))
		     (spawn-player player dungeon))
		    'indungeon)
		   ('indungeon
		    (format t "in dungeon...~%")
		    (do-update-dungeon console dungeon))
		   ('death
		    (format t "death...~%")
		    (do-death console dungeon))
		   ('win
		    (format t "win...~%")
		    (do-win console dungeon))
		   ('end
		    (setf should-exit t))
		   (otherwise
		    (format t "starting...~%")
		    (do-intro console)))))))
  (format t "exiting...~%"))

(defun do-intro (console)
  "Display intro and wait for key to proceed to chargen."
  (clear-console console)
  (display-text-wrapped console 13 15 40 10 +INTRO-TEXT+)
  (update-console console)
  (wait-for-keypress +key-space+ console)
  (clear-console console)
  (update-console console)
  'chargen)

(defun do-chargen (console)
  "Player character generation."
  (make-lifeform 
   :name "teszt player"
   :gender 'm
   :role 'knight
   :alignment 'chaothic-good
   :deity 'zumba
   :inventory '()
   :armor '()
   :weapons '()
   :tools '()
   :hp 100 :maxhp 100
   :aktlevel 0))

(defun gen-levels (player)
  "Generate dungeon levels for player."
  (list (convert-test-level "test level" +TESTLEVEL+)))

(defun get-player-level (dungeon)
  (nth (lifeform-aktlevel (dungeon-player dungeon)) (dungeon-levels dungeon)))

(defun spawn-player (player dungeon)
  "Place player on the first level in dungeon by the ladder upwards (entry)."
  (setf (lifeform-aktlevel player) 0)
  (let ((upladders (find-dungeonfeatures-in-level (get-player-level dungeon) ">")))
    (setf (lifeform-x player) (object-x (first upladders)))
    (setf (lifeform-y player) (object-y (first upladders)))))

(defun do-update-dungeon (console dungeon)
  (let ((key (wait-for-any-key console)))
    (clear-console console)
    (update-console console)
    (cond ((key-eq key +key-q+)
	   'end)
	  ((key-eq key +key-space+)
	   (format t "*")
	   (player-idle-turn console dungeon)
	   (update-console console)
	   'indungeon)
	  ((key-eq key +key-h+)
	   (format t "help~%")
	   (display-help console)
	   'indungeon))))

(defun player-idle-turn (console dungeon)
  (incf (dungeon-turns dungeon))
  (let ((player (dungeon-player dungeon))
	(level (get-player-level dungeon)))
   (update-idle-player player)
   (update-level level)
   (draw-level console level)
   (draw-player console player))
  )

(defun display-help (console))

(defun do-death (console dungeon)
  'end)

(defun do-win (console dungeon)
  'end)

(defun update-idle-player (player))

(defun update-level (level))

(defun draw-level (console level)
  (draw-map console (level-map level))
  (draw-items console (level-items level))
  (draw-monsters console (level-monsters level)))

(defun draw-map (console map))

(defun draw-items (console items))

(defun draw-monsters (console monstas))

(defun draw-player (console player))
