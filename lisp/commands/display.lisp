(in-package :stumpwm)
(ql:quickload :zpng)

;; Code for Displaying Screenshots

(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defun take-screenshot (&optional (host ""))
  (with-display host (display screen root-window)
    (xlib:get-image root-window :x 0 :y 0 :width (xlib:screen-width screen) :height (xlib:screen-height screen))))

(xlib:get-i)


(ql:quickload :clx)

(defun pop-up-window (&optional (host ""))
  (let* ((display (xlib:open-display host))
         (screen (first (xlib:display-roots display)))
         (root-window (xlib:screen-root screen)))
    (xlib:get-image root-window :x 0 :y 0 :width 2160 :height 3840)))


    ;; (root-window (xlib:screen-root screen))
    ;; (my-window (xlib:create-window
    ;;             :parent root-window
    ;;             :x 0
    ;;             :y 0
    ;;             :width 200
    ;;             :height 300)))
  ;; (xlib:map-window my-window)
  ;; (xlib:display-finish-output display)))

;; (setf win (pop-up-window "")
;;       (describe 'xlib:map-window)
;;       (my-window (xlib:create-window
;;                                          :parent root-window
;;                                          :x 0
;;                                          :y 0
;;                                          :width 200
;;                                          :height 300))

;; (xlib:get-raw-image )

;; (sleep life-time)
;; (xlib:destroy-window win)
;; (xlib:close-display display)))


(setf testvar (take-screenshot))
(xlib:save-s)

(xlib:image-x)
(display-screenshot testvar)

(print testvar)
(ql:quickload '(:zpng :png-read :cffi))
(use-package :zpng)

(save-screenshot-as-png (take-screenshot) "test.png" )

;; (display-screenshot testvar)
(xlib:image-width testvar)
(xlib:pixarray)

(type-of testvar)
(xlib:image-z-pixarray testvar)

(defun save-screenshot-as-png (image path)
  (with-display "" (display screen root-window)
    (let* ((width (xlib:image-width image)) (height (xlib:image-height image))
           (drawable (xlib:create-window :parent root-window
                                         :x 0 :y 0 :width (xlib:image-width image) :height (xlib:image-height image)
                                         :event-mask (xlib:make-event-mask :exposure :key-press :button-press)))
           (png (make-instance 'zpng:png :width width :height height
                                         :color-type :truecolor-alpha
                                         :image-data (xlib:image-pixmap drawable image)))
           (data (zpng:data-array png)))
      (dotimes (y height (zpng:write-png png path))
        (dotimes (x width)
          (rotatef (aref data y x 0) (aref data y x 2)) ; BGR -> RGB
          (setf (aref data y x 3) 255))))))

(defvar *window-loop* t)

(defun display-screenshot (image &optional (host ""))
  (with-display host (display screen root-window)
    (let* ((window (xlib:create-window :parent root-window
                                       :x 0 :y 0 :width 1920 :height 1080
                                       :event-mask (xlib:make-event-mask :exposure :key-press :button-press)))
           (gcontext (xlib:create-gcontext :drawable window)))
      (defparameter *test-win* window)
      (bt:make-thread #'(lambda ()
                          (loop while *window-loop*
                                do (progn (xlib:map-window window) (sleep 1)))))
      (unwind-protect
           (handler-case
               (xlib:event-case (display :force-output-p t :discard-p t)
                 (:exposure () (xlib:put-image window gcontext image :x 0 :y 0))
                 (t () t))
             (end-of-file ()))
        (xlib:destroy-window window)))))


(display-screenshot (take-screenshot))

(remove-head
 (workspace-sceen (group-workspace g))
 (setf *frame-to-delete* (car (sort (screen-heads (current-screen))
             #'(lambda (x y)
                 (< (head-width x) (head-width y)))))))


(mapcar #'(lambda (g) (remove-head ))
 (flat-list (mapcar #'workspace-groups  (hash-table-values workspace-hash ))))
(space-this 4)


(mapcar #'(lambda (x)
            (remove-head x *frame-to-delete*))
            (cons (current-screen)
                  (mapcar #'ws-screen
                          (hash-table-values workspace-hash)))
