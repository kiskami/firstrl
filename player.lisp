;;; player.lisp
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

(defun add-msg (console msg)
  (display-text-wrapped console 1 27 (- (consoledata-w console) 2) 2 msg))

(defun display-hud (console player)
  (display-text console 1 30 (format nil "@ Player ~A ~A ~A ~A HP:~A/~A PW:~A/~A Xp:~A $:~A"
				     (lifeform-name player)
				     (if (equal 'm (lifeform-gender player)) "male" "female")
				     (lifeform-alignment player)
				     (lifeform-role player)
				     (lifeform-hp player) (lifeform-maxhp player)
				     (lifeform-power player) (lifeform-maxpower player)
				     (lifeform-xp player)
				     (lifeform-purse player)))
  (display-text console 1 31 (format nil "Str:~A/~A Dex:~A/~A Con:~A/~A Int:~A/~A Wis:~A/~A Cha:~A/~A Turn:~A ~A"
				     (lifeform-str player) (lifeform-maxstr player)
				     (lifeform-dex player) (lifeform-maxdex player)
				     (lifeform-con player) (lifeform-maxcon player)
				     (lifeform-int player) (lifeform-maxint player)
				     (lifeform-wis player) (lifeform-maxwis player)
				     (lifeform-cha player) (lifeform-maxcha player)
				     (lifeform-turns player)
				     (lifeform-state player))))

(defun draw-player (console player &key (dx 1) (dy 1) (draw-hud t))
;  (display-text console 0 27 "1")
;  (display-text console 0 28 "2")
  (when draw-hud
    (display-hud console player))
  (display-text console 0 29 "-")
;  (display-text console 0 30 "4")
;  (display-text console 0 31 "5")
  (display-text console 0 32 "-")
  (display-text console 0 33 "-")
  (display-text console (+ dx (lifeform-x player)) (+ dy (lifeform-y player)) (playerdata-char *playerdata*))
  )

(defun move-player (dungeon dir)
  (let* ((player (dungeon-player dungeon)) 
	 (dkords (can-player-move player (get-player-level dungeon) dir)))
    (cond (dkords
	   (incf (lifeform-x player) (car dkords))
	   (incf (lifeform-y player) (cdr dkords)))
	  (t ; something in the way
	   ; if monster, then attack
	   ; if non walkable feature or item ...
	   )
	  )
    (incf (lifeform-turns player))))

(defun can-player-move (player level dir)
(let ((d (nth dir dkords)))
  (when (equal #\. 
	       (aref (level-map level) 
		     (+ (lifeform-x player) (car d))
		     (+ (lifeform-y player) (cdr d))))
    d))
  )

(defun move-player-down (dungeon))

(defun move-player-up (dungeon))
