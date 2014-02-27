;;; console.lisp

(in-package #:firstrl)

(defconstant +key-space+ :SDL-KEY-SPACE)
(defconstant +key-q+ :SDL-KEY-Q)

(defstruct consoledata
  w h fullscreen resizable
  windowsurf)

(defun init-fonts ()
  (setf *FONTMAP* (list (cons 'sans (make-instance 'SDL:ttf-font-definition
				    :size +TILESIZE+
				    :filename (merge-pathnames "DroidSans.ttf")))
	 (cons 'sans-bold (make-instance 'SDL:ttf-font-definition
					 :size +TILESIZE+
					 :filename (merge-pathnames "DroidSans-Bold.ttf")))
	 (cons 'serif (make-instance 'SDL:ttf-font-definition
				     :size +TILESIZE+
				     :filename (merge-pathnames "DroidSerif-Regular.ttf")))
	 (cons 'serif-bold (make-instance 'SDL:ttf-font-definition
					  :size +TILESIZE+
					  :filename (merge-pathnames "DroidSerif-Bold.ttf"))))))

(defmacro with-init-console (&rest body)
  (sdl:load-library)
  (init-fonts)
  `(sdl:WITH-INIT () ,@body))

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
  (format t "Waiting for key ~A..." key)
  (loop 
     (sdl:with-events (:WAIT)
       (:key-down-event (:key k)
			(when (key-eq k key)
			  (format t "pressed.~%")
			  (return-from wait-for-keypress k))))))

(defun wait-for-any-key (console)
  (declare (ignore console))
  (format t "Waiting for any key...")
  (sdl:with-events (:WAIT)
    (:key-down-event (:key k)
		     (format t "~A pressed.~%" k)
		     (return-from wait-for-any-key k))))

(defun key-eq (key1 key2)
  (sdl:key= key1 key2))

(defun display-text (console x y w h text &optional (halign 'left) (font 'sans))
  "Draw text, wrap if nedded and there is enough space."
  (sdl:with-font (f (get-font font))
    (sdl:draw-string-solid-* text x y :surface (consoledata-windowsurf console) :color +DEFCOLOR+)))

(defun clear-console (console)
  (declare (ignore console))
  (sdl:clear-display sdl:*black*))

(defun update-console (console)
  (declare (ignore console))
  (sdl:update-display))
