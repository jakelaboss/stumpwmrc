(in-package :stumpwm)

;; Code for Displaying Screenshots

(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
          ;; (,screen (first (xlib:display-roots ,display)))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defun take-screenshot (&optional (host ""))
  (with-display host (display screen root-window)
    (xlib:get-image root-window :x 0 :y 0 :width (xlib:screen-width screen) :height (xlib:screen-height screen))))

(defun save-screenshot-as-png (image path)
  (let* ((width (xlib:image-width image)) (height (xlib:image-height image))
         (png (make-instance 'zpng:png :width width :height height
                                       :color-type :truecolor-alpha
                                       :image-data (xlib:image-z-pixarray image)))
         (data (zpng:data-array png)))
    (dotimes (y height (zpng:write-png png path))
      (dotimes (x width)
        (rotatef (aref data y x 0) (aref data y x 2)) ; BGR -> RGB
        (setf (aref data y x 3) 255)))))

(defun display-screenshot (image &optional (host ""))
  (with-display host (display screen root-window)
    (let* ((window (xlib:create-window :parent root-window
                                       :x 0 :y 0 :width (xlib:image-width image) :height (xlib:image-height image)
                                       :event-mask (xlib:make-event-mask :exposure :key-press :button-press)))
           (gcontext (xlib:create-gcontext :drawable window)))
      (xlib:map-window window)
      (unwind-protect
           (handler-case
               (xlib:event-case (display :force-output-p t :discard-p t)
                 (:exposure () (xlib:put-image window gcontext image :x 0 :y 0))
                 (t () t))
             (end-of-file ()))
        (xlib:destroy-window window)))))
