;;; console.lisp
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

(defconstant +key-b+ :SDL-KEY-B)
(defconstant +key-h+ :SDL-KEY-H)
(defconstant +key-i+ :SDL-KEY-I)
(defconstant +key-j+ :SDL-KEY-J)
(defconstant +key-k+ :SDL-KEY-K)
(defconstant +key-l+ :SDL-KEY-L)
(defconstant +key-m+ :SDL-KEY-M)
(defconstant +key-n+ :SDL-KEY-N)
(defconstant +key-q+ :SDL-KEY-Q)
(defconstant +key-u+ :SDL-KEY-U)
(defconstant +key-y+ :SDL-KEY-Y)

(defconstant +key-space+ :SDL-KEY-SPACE)
(defconstant +key-<+ :SDL-KEY-LESS)
(defconstant +key->+ :SDL-KEY-GREATER)
(defconstant +key-.+ :SDL-KEY-PERIOD)

(defparameter *FONTMAP* nil "Fonts used by this game.")
(defparameter *GLYPCACHE* (make-hash-table :test #'equal) "Prerendered char glyp surfaces.")

(defstruct consoledata
  w h fullscreen resizable
  windowsurf)

(defun init-fonts ()
  (setf *FONTMAP* (list (cons 'sans (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									:size +NORMAL-FONT-SIZE+
									:filename (merge-pathnames "VeraMono.ttf"))))
			(cons 'sans-small (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									      :size +SMALL-FONT-SIZE+
									      :filename (merge-pathnames "VeraMono.ttf"))))
			(cons 'sans-big (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									      :size +BIG-FONT-SIZE+
									      :filename (merge-pathnames "VeraMono.ttf"))))
			(cons 'sans-bold (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									     :size +NORMAL-FONT-SIZE+
									     :filename (merge-pathnames "VeraMono-Bold.ttf")))))))

(defun add-to-glypcache (char surf)
  (setf (gethash char *GLYPCACHE*) surf))

(defun get-from-glypcache (char)
  (gethash char *GLYPCACHE*))

(defmacro with-init-console (&rest body)
  (sdl:load-library)
  `(sdl:WITH-INIT ()
       (init-fonts)
       ,@body))

(defun create-window (w h title &key (fullscreen nil) (resizable nil))
  "Create SDL window, and return a state that the other funcs use for rendering, input, etc."
  (sdl:color :r 0 :g 0 :b 0 :a 1)
  (make-consoledata 
   :w w :h h
   :fullscreen fullscreen
   :resizable resizable
   :windowsurf (sdl:WINDOW (* w +TILESIZE+) 
		(* h +TILESIZE+)
		:fullscreen fullscreen :double-buffer t
		:resizable resizable ;:no-frame t
		:title-caption title
		:icon-caption title)))

(defun wait-for-keypress (key console)
  (declare (ignore console))
;  (format t "Waiting for key ~A..." key)
  (loop 
     (sdl:with-events (:WAIT)
       (:key-down-event (:key k)
			(when (key-eq k key)
;			  (format t "pressed.~%")
			  (return-from wait-for-keypress k))))))

(defun wait-for-any-key (console)
  (declare (ignore console))
;  (format t "Waiting for any key...")
  (sdl:with-events (:WAIT)
    (:key-down-event (:key k)
;		     (format t "~A pressed.~%" k)
		     (return-from wait-for-any-key k))))

(defun key-eq (key1 key2)
  (sdl:key= key1 key2))

(defun get-font (key)
  (let ((ent (assoc key *FONTMAP*)))
    (if ent
	(rest ent)
	(error "Fontdef not found."))))

(defun cache-char-glyp (char &key (font 'sans) (color +DEFCOLOR+))
  (let ((f (get-font font)))
   (add-to-glypcache char (cons (sdl:render-string-blended char :font f :color color) f))))

(defun display-char-glyp (console x y char)
  (let* ((entry (get-from-glypcache char)) 
	 (surf (car entry)) 
	 (font (cdr entry)))
    (sdl:draw-surface-at-* surf
			   (* x +TILESIZE+)
			   (* y (sdl:get-font-line-skip :font font))
			   :surface (consoledata-windowsurf console))))

(defun display-text-on-surf (x y text &key (font 'sans) surf (color +DEFCOLOR+))
  (sdl:draw-string-blended-* text x y
  			   :surface surf
  			   :color color
  			   :font (get-font font)))

(defun display-text (console x y text &key (font 'sans) (color +DEFCOLOR+))
  (let ((f (get-font font)))
   (display-text-on-surf (* x +TILESIZE+)
			 (* y (sdl:get-font-line-skip :font f)) 
			 text 
			 :font font 
			 :surf (consoledata-windowsurf console)
			 :color color)))

(defun display-text-wrapped (console x y w h text &key (font 'sans) (color +DEFCOLOR+))
  "Display text in the rectangular region [(x,y)(x+w,y+h)] with simple line wrapping."
  (let* ((line "") 
	 (f (get-font font))
	 (font-line-skip (sdl:get-font-line-skip :font f))
	 (liney (* y font-line-skip)))
    (labels ((dt (x y tx fo s) (display-text-on-surf x y tx :font fo :surf s :color color)))
     (dolist (word (split-by text))
       (cond ((> (sdl:get-font-size (format nil "~A ~A" line word) :size :W :font f) (* w +TILESIZE+))
	      ;; sor kiírása
;	     (format t "display text: x:~A liney:~A line:~A~%" (* x +TILESIZE+) liney line)
	      (dt (* x +TILESIZE+) liney line font (consoledata-windowsurf console))
	      ;; új sor kell
	      (incf liney font-line-skip)
	      (setf line word)
	      (if (> liney (* (+ y h) font-line-skip)) (return-from display-text-wrapped)))
	     (t
;	     (format t "adding to line, line:~A word:~A~%" line word)
	      (setf line (format nil "~A ~A" line word)))))
     ;; utolsó sort még ki kell írni
     (dt (* x +TILESIZE+) liney line font (consoledata-windowsurf console)))))

(defun clear-console (console)
  (declare (ignore console))
  (sdl:clear-display sdl:*black*))

(defun update-console (console)
  (declare (ignore console))
  (sdl:update-display))
