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

(defun display-hud_ (console player)
  (display-text console 1 32 (format nil "@ Player ~A ~A ~A ~A HP:~A/~A PW:~A/~A Xp:~A $:~A"
				     (lifeform-name player)
				     (if (equal 'm (lifeform-gender player)) "male" "female")
				     (lifeform-alignment player)
				     (lifeform-role player)
				     (lifeform-hp player) (lifeform-maxhp player)
				     (lifeform-power player) (lifeform-maxpower player)
				     (lifeform-xp player)
				     (lifeform-purse player)))
  (display-text console 1 33 (format nil "Str:~A/~A Dex:~A/~A Con:~A/~A Int:~A/~A Wis:~A/~A Cha:~A/~A Turn:~A ~A"
				     (lifeform-str player) (lifeform-maxstr player)
				     (lifeform-dex player) (lifeform-maxdex player)
				     (lifeform-con player) (lifeform-maxcon player)
				     (lifeform-int player) (lifeform-maxint player)
				     (lifeform-wis player) (lifeform-maxwis player)
				     (lifeform-cha player) (lifeform-maxcha player)
				     (lifeform-turns player)
				     (lifeform-state player))))

(defun display-hud (console player)
  (display-text console 1 33 (format nil "@ Player ~A ~A ~A HP:~A/~A Xp:~A $:~A  Att:~A Def:~A Turn:~A ~A"
				     (lifeform-name player)		
				     (if (equal 'm (lifeform-gender player)) "male" "female")
				     (lifeform-role player)
				     (lifeform-hp player) (lifeform-maxhp player)
				     (lifeform-xp player)
				     (lifeform-purse player)
				     (lifeform-att player) (lifeform-def player)
				     (lifeform-turns player)
				     (lifeform-state player))))

(defun draw-player (console player &key (dx 1) (dy 1) (draw-hud t))
  (display-text console 0 27 "1")
  (display-text console 0 28 "2")
  (display-text console 0 29 "3")
  (display-text console 0 30 "4")
  (display-text console 0 31 "5")
  (when draw-hud
    (display-hud console player))
  (display-text console 0 32 "6")
;  (display-text console 0 33 "7")
  (display-text console (+ dx (lifeform-x player)) (+ dy (lifeform-y player)) (playerdata-char *playerdata*))
  )

(defun move-player (dungeon dir)
  (let* ((player (dungeon-player dungeon))
	 (level (get-player-level dungeon))
	 (newkords (get-newkords player dir)))
    (when (is-floor (level-map level) (car newkords) (cdr newkords))
      (let ((o (is-object-at (car newkords) (cdr newkords)
			     (level-monsters level)))
	    (canstep nil))
	; monsta? fight!
	(setf canstep (if o (monsta-fight player o) t))

	(when canstep
	  (setf o (is-object-at (car newkords) (cdr newkords)
				(level-items level)))
	  ; item? pick up!
	  (setf canstep (if o (item-pickup player o) t))

	  (when canstep         
	    (setf o (is-object-at (car newkords) (cdr newkords)
				  (level-features level)))
	    ; dungeonfeature? handle
	    (setf canstep (if o (feature-handle player o) t))))

	(when canstep
	  (setf (lifeform-x player) (car newkords)
		(lifeform-y player) (cdr newkords)))))))

(defun get-newkords (player dir)
  (let ((d (nth dir dkords)))
    (cons (+ (lifeform-x player) (car d))
	  (+ (lifeform-y player) (cdr d)))))

(defun monsta-fight (player monsta)
  (format t "stepping into a monsta! fight!~%")
  t
  )

(defun item-pickup (player item)
  (format t "item pickup.~%")
  t
  )

(defun feature-handle (player fea)
  (format t "dungeon feature in the way...~%")
  t
  )
