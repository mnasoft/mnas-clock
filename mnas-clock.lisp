;;;; mnas-clock.lisp

(defpackage #:mnas-clock
  #+nil (:use #:cl)
  ;;#+nil
  (:use #:cl #:kit.sdl2 #:kit.gl.shader #:kit.math) ;; #:alexandria
  (:export #:make-simple-window))
  
(in-package #:mnas-clock)

(defparameter *pi*    (coerce pi         'single-float))
(defparameter *2pi*   (coerce (* 2 pi)   'single-float))
(defparameter *pi/2*  (coerce (/ pi 2)   'single-float))
(defparameter *pi/3*  (coerce (/ pi 3)   'single-float))
(defparameter *pi/4*  (coerce (/ pi 4)   'single-float))
(defparameter *pi/6*  (coerce (/ pi 6)   'single-float))
(defparameter *pi/12* (coerce (/ pi 12)  'single-float))
(defparameter *deg*   (coerce (/ pi 180) 'single-float))

(defclass test-window (gl-window)
  ((start-time :initform (get-internal-real-time))
   (frames :initform 0)))

(defmethod render :after ((window test-window))
  (with-slots (start-time frames) window
    (let* ((interval 5)
           (current-time (get-internal-real-time))
           (seconds (/ (- current-time start-time) internal-time-units-per-second))
           (fps (float (/ frames interval))))
      (incf frames)
      (when (and (> seconds interval)
                 (plusp fps))
        (format t "Framerate: ~,3f fps, ~,3f ms/frame~%" fps (/ 1000 fps))
        (setf frames 0
              start-time current-time)))))

(defmethod textinput-event :after ((window test-window) ts text)
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defmethod keyboard-event :after ((window test-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (close-window window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simple-window (test-window)
  ((rotation :initform 0.0)))

(defmethod initialize-instance :after ((w simple-window) &key &allow-other-keys)
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.
  (setf (sdl2.kit:idle-render w) t)
  (gl:viewport 100 0 600 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod render ((window simple-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW
  ;; after RENDER.
  (with-slots (rotation) window
    (let* ((w (window-width window))
           (h (window-height window))
           (sz (min w h)))
    ;;
      (gl:viewport (/ (- w sz) 2 ) (/ (- h sz) 2 )
                   sz sz ))
;;  (gl:matrix-mode :projection)
;;  (gl:ortho -2 2 -2 2 -2 2)
;;  (gl:matrix-mode :modelview)
    ;;
    
    (gl:load-identity)
    (gl:rotate rotation 0 0 1)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer)
    (gl:begin :polygon)
    (gl:color 1.0 1.0 1.0)
    (loop :for i :from 0 :to 100 :do
      (gl:vertex (* 1.5 (sin (* *2pi* (/ i 100))))
                 (* 1.5 (cos (* *2pi* (/ i 100))))))
    (gl:end)
    (gl:point-size 10)
    (gl:enable :point-smooth)
    (gl:begin :points)
    (gl:color 0.0 0.0 0.0)
    (loop :for i :from 0 :to 12 :do
      (gl:vertex (* 1.4 (sin (* *2pi* (/ i 12))))
                 (* 1.4 (cos (* *2pi* (/ i 12))))))
    (gl:vertex 0.0  0.0 0.0)
    (multiple-value-bind (ss mm hh) (decode-universal-time (get-universal-time))
      (gl:color 0.0 0.0 0.0)
      (gl:vertex (* 1.4 (sin (* *2pi* (/ ss 60))))
                 (* 1.4 (cos (* *2pi* (/ ss 60)))))
      (gl:color 0.0 1.0 0.0)
      (gl:vertex (* 1.3 (sin (* *2pi* (/ (+ mm (/ ss 60)) 60))))
                 (* 1.3 (cos (* *2pi* (/ (+ mm (/ ss 60)) 60)))))
      (gl:color 1.0 0.0 1.0)
      (gl:vertex (* 1.2 (sin (* *2pi* (/ (mod (+ hh (/ mm 60) (/ ss 3600)) 12) 12))))
                 (* 1.2 (cos (* *2pi* (/ (mod (+ hh (/ mm 60) (/ ss 3600)) 12) 12))))))
    (gl:end)
    (gl:begin :triangles)

    (multiple-value-bind (ss mm hh) (decode-universal-time (get-universal-time))
      (let ((ss-a (- *pi/2* (* ss 2/60 3.1415927)))
            (mm-a (- *pi/2* (* (+ mm (/ ss 60)) 2/60 3.1415927)))
            (hh-a (- *pi/2* (* (mod (+ hh (/ mm 60) (/ ss 3600)) 12) 2/12 3.1415927))))
        (gl:color 1.0 0.0 1.0)
        (apply 'gl:vertex (vlisp:polar '(0 0) (+ hh-a *pi/2*) 0.06))
        (apply 'gl:vertex (vlisp:polar '(0 0) hh-a 1.0))
        (apply 'gl:vertex (vlisp:polar '(0 0) (- hh-a *pi/2*) 0.06))
        (gl:color 0.0 1.0 0.0)
        (apply 'gl:vertex (vlisp:polar '(0 0) (+ mm-a *pi/2*) 0.05))
        (apply 'gl:vertex (vlisp:polar '(0 0) mm-a 1.2))
        (apply 'gl:vertex (vlisp:polar '(0 0) (- mm-a *pi/2*) 0.05))
        (gl:color 1.0 0.0 0.0)
        (apply 'gl:vertex (vlisp:polar '(0 0) (+ ss-a *pi/2*) 0.04))
        (apply 'gl:vertex (vlisp:polar '(0 0) ss-a 1.4))
        (apply 'gl:vertex (vlisp:polar '(0 0) (- ss-a *pi/2*) 0.04))
        (gl:end)))))

(defmethod close-window ((window simple-window))
  (format t "Bye!~%")
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

(defmethod mousewheel-event ((window simple-window) ts x y)
  (with-slots (rotation) window
    (incf rotation (* 12 y))
    (render window)))

(defmethod textinput-event ((window simple-window) ts text)
  (when (string= "W" (string-upcase text))
    (setf (window-height window) (+ (window-height window) 10)))
  (when (string= "S" (string-upcase text))
    (setf (window-height window) (- (window-height window) 10)))
  (when (string= "E" (string-upcase text))
    (setf (window-width  window) (+ (window-width window)  10)))
  (when (string= "D" (string-upcase text))
    (setf (window-width  window) (- (window-width window)  10)))
  (format t "You typed: ~S~%" text))

(defmethod keyboard-event ((window simple-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (unless repeat-p
      (format t "~A ~S ~S~%" state scancode (sdl2:scancode-name scancode)))))

(defmethod mousebutton-event ((window simple-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))

(defmethod mousemotion-event ((window simple-window) ts mask x y xr yr)
  (when (> mask 0)
    (format t "Mouse motion, button-mask = ~A at ~A, ~A~%" mask x y)))

(defmethod controller-added-event ((window simple-window) c)
  (format t "Added ~A (id=~A)~%" c (sdl2:game-controller-instance-id c)))

(defmethod controller-removed-event ((window simple-window) c)
  (format t "Removed ~A (id=~A)~%" c (sdl2:game-controller-instance-id c)))

(defmethod controller-axis-motion-event ((window simple-window) c ts axis value)
  (format t "ID ~A, Axis ~A, Value ~A~%"
          (sdl2:game-controller-instance-id c) axis value))

(defmethod controller-button-event ((window simple-window) c state ts button)
  (format t "ID ~A, Button ~A, State ~S~%"
          (sdl2:game-controller-instance-id c) button state))

(defun make-simple-window ()
  (kit.sdl2:start)
  (make-instance 'simple-window))

;; (make-instance 'simple-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; (kit.sdl2:start)

;;;;(make-instance 'simple-window)
