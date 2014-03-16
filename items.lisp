;;; items.lisp
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

(defun gen-items (leveldata map features)
  (let ((res ()))
    (dolist (i (leveldata-items leveldata))
      (setf res (append res
			(apply #'gen-items*
			       leveldata map features
			       i))))
    res))

(defun gen-items* (ld m fea &key typeid cnt (sickprob 0) 
			     (hp 0) (att 0) (def 0) (spe 0) (xp 0))
  (let ((res ()))
    (dotimes (i cnt res)
      (let ((id (gethash typeid *itemdata*))
	    (kord nil))
	(cond (id
	       (setf
		kord (find-kord-for-item ld m fea res)
		res (append res
			    (list
			     (make-objectwithrole 
			      :name (itemdata-name id)
			      :typeid typeid
			      :x (car kord)
			      :y (cdr kord)
			      :state (if (>= sickprob (get-rnd-number 0 100))
					 'sick 'normal)
			      :hp hp :att att :def def :spe spe :xp xp)
			     ))))
	      (t
	       (error "Itemdata not found.")))
	)
      ))
  )
  
(defun find-kord-for-item (ld map fea il)
    (do ((x 0 (get-rnd-number 1 (1- (leveldata-w ld))))
       (y 0 (get-rnd-number 1 (1- (leveldata-h ld)))))
      ((and (is-floor map x y)
	    (not (is-object-at x y fea)) ; feature there?
	    (not (is-object-at x y il))	; item there?
	    ) (cons x y))
    ;empty
    ))
