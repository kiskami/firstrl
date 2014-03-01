;;; console.lisp

(in-package #:firstrl)

(defconstant +key-space+ :SDL-KEY-SPACE)
(defconstant +key-q+ :SDL-KEY-Q)

(defvar *FONTMAP* nil "Fonts used by this game.")

(defstruct consoledata
  w h fullscreen resizable
  windowsurf)

(defun init-fonts ()
  (setf *FONTMAP* (list (cons 'sans (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									:size +NORMAL-FONT-SIZE+
									:filename (merge-pathnames "DroidSans.ttf"))))
			(cons 'sans-small (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									      :size +SMALL-FONT-SIZE+
									      :filename (merge-pathnames "DroidSans.ttf"))))
			(cons 'sans-big (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									      :size +BIG-FONT-SIZE+
									      :filename (merge-pathnames "DroidSans.ttf"))))
			(cons 'sans-bold (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									     :size +NORMAL-FONT-SIZE+
									     :filename (merge-pathnames "DroidSans-Bold.ttf"))))
			(cons 'serif (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									 :size +NORMAL-FONT-SIZE+
									 :filename (merge-pathnames "DroidSerif-Regular.ttf"))))
			(cons 'serif-bold (sdl:initialise-font (make-instance 'SDL:ttf-font-definition
									      :size +NORMAL-FONT-SIZE+
									      :filename (merge-pathnames "DroidSerif-Bold.ttf")))))))

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
    (if (not (null ent))
	(rest ent)
	(error "Fontdef not found."))))

(defun display-text (x y text &key (font 'sans) surf)
  (sdl:draw-string-solid-* text x y
  			   :surface surf
  			   :color +DEFCOLOR+ 
  			   :font (get-font font)))

(defun display-text-wrapped (console x y w h text &key (font 'sans))
  "Display text in the rectangular region [(x,y)(x+w,y+h)] with simple line wrapping."
  (let* ((line "") 
	 (f (get-font font))
	 (font-line-skip (sdl:get-font-line-skip :font f))
	 (liney (* y font-line-skip)))
    (labels ((dt (x y tx fo s) (display-text x y tx :font fo :surf s)))
     (dolist (word (split-by-white-space text))
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
