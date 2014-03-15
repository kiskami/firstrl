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
  (display-text console 0 32 "6")
  (display-msg-window console 1 27)
  (when draw-hud
    (display-hud console player))
;  (display-text console 0 33 "7")
  (display-text console (+ dx (lifeform-x player)) (+ dy (lifeform-y player)) (playerdata-char *playerdata*))
  )

(defun move-player (console dungeon dir)
  (let* ((player (dungeon-player dungeon))
	 (level (get-player-level dungeon))
	 (newkords (get-newkords player dir)))
    (incf (lifeform-turns player))
    (when (is-floor (level-map level) (car newkords) (cdr newkords))
      (let ((o (is-object-at (car newkords) (cdr newkords)
			     (level-monsters level)))
	    (canstep nil))
	; monsta? fight!
	(setf canstep (if (and o (monsta-alive o))
			  (monsta-fight level console player o) t))

	(when canstep
	  (setf o (is-object-at (car newkords) (cdr newkords)
				(level-items level)))
	  ; item? pick up!
	  (setf canstep (if o (item-pickup console player o) t))

	  (when canstep         
	    (setf o (is-object-at (car newkords) (cdr newkords)
				  (level-features level)))
	    ; dungeonfeature? handle
	    (setf canstep (if o (feature-handle console player o) t))))

	(when canstep
	  (setf (lifeform-x player) (car newkords)
		(lifeform-y player) (cdr newkords)))
	(return-from move-player canstep)))))

(defun get-newkords (player dir)
  (let ((d (nth dir dkords)))
    (cons (+ (lifeform-x player) (car d))
	  (+ (lifeform-y player) (cdr d)))))

(defun monsta-fight (level console player monsta)
;  (format t "stepping into a monsta! fight!~%")
  (let ((player-spe (lifeform-spe player))
	(monsta-spe (lifeform-spe monsta))
	)
    ; compare speed - faster side attacks first
    (cond ((>= player-spe monsta-spe)
	   (if (> player-spe 0)
	       (player-attack level player monsta)
	       (add-msg "Too slow, you can't attack!"))
	   (when (monsta-alive monsta) 
	     (if (> monsta-spe 0)
		 (monsta-attack console player monsta)
		 (add-msg (format nil "The ~A doesn't fight back!" (lifeform-name monsta)))))
	   )
	  (t
	   (monsta-attack console player monsta)
	   (when (player-alive player) 
	     (if (> player-spe 0)
		 (player-attack level player monsta)
		 (add-msg "Too slow, you can't fight back!")))
	   )	
	  ))
  (and (player-alive player) (not (monsta-alive monsta)))
  )

(defun item-pickup (console player item)
  (format t "item pickup.~%")
  t
  )

(defun feature-handle (console player fea)
  (format t "dungeon feature in the way...~%")
  t
  )

(defun player-attack (level player monsta)
  (let ((patt (get-rnd-number 0 (lifeform-att player)))
	(mdef (get-rnd-number 0 (lifeform-def monsta))))
    (when (and (player-alive player) (monsta-alive monsta))
;      (format t "player attack~%")
      (cond ((and (> patt 0) (> patt mdef))
	     (add-msg (format nil "You hit the ~A, and wound ~A hp."
			       (lifeform-name monsta)
			       (- patt mdef)))
	     (damage-monster level monsta (- patt mdef))
	     )
	    ((and (> patt 0) (= patt mdef))
	     (add-msg (format nil "The ~A wards off your attack barely."
			       (lifeform-name monsta)))
	     )
	    ((and (> patt 0) (< patt mdef))
	     (add-msg (format nil
			       "You try to hit the ~A, but it evades you easily."
			       (lifeform-name monsta)))
	     )
	    ((= patt 0)
	     (add-msg (format nil "This was a crippled attempt from you.")))
	    )
      )))

(defun monsta-attack (console player monsta)
  (let ((matt (get-rnd-number 0 (lifeform-att monsta)))
	(pdef (get-rnd-number 0 (lifeform-def player))))
    (when (and (player-alive player) (monsta-alive monsta))
;      (format t "monsta attack~%")
      (cond ((and (> matt 0) (> matt pdef))
	     (add-msg (format nil "The ~A hits you, and wound ~A hp."
			       (lifeform-name monsta)
			       (- matt pdef)))
	     (damage-player console player (- matt pdef))
	     )
	    ((and (> matt 0) (= matt pdef))
	     (add-msg (format nil "You ward off the attack of the ~A barely."
			       (lifeform-name monsta)))
	     )
	    ((and (> matt 0) (< matt pdef))
	     (add-msg (format nil
			       "The ~A try to hit you, but you evade easily."
			       (lifeform-name monsta)))
	     )
	    ((= matt 0)
	     (add-msg (format nil "This was a crippled attempt from the ~A."
			       (lifeform-name monsta))))
	    )
      )))

(defun damage-player (console ply dmg)
    (cond ((>= dmg (lifeform-hp ply))
	 (setf (lifeform-hp ply) 0
	       (lifeform-state ply) 'dead)
	   (add-msg (format nil "*** You die from the attack. Press any key to quit. ***"))
	   (wait-for-any-key console)
	 )
	(t
	 (decf (lifeform-hp ply) dmg)
	 (when (<= (lifeform-hp ply) (/ (lifeform-maxhp ply) 3))
	   (add-msg (format nil "You are heavily wounded.")))
	 )))

(defun player-alive (ply)
  (not (equal (lifeform-state ply) 'dead)))
