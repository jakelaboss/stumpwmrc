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

(in-package :stumpwm)

(let ((frame (current-frame)))
  (grab-rgb-from-window (window-xwin (frame-window frame))
                        (- 10 (frame-x frame)) (- 10 (frame-y frame))))

(ql:quickload '(:imago :clx :stumpwm))

(ql:quickload :jpeg-turbo)

(defvar test-image (imago:read-png "/home/vagabond/test.png"))

(slot-value test-image 'imago:image-colormap )

(imago:image-colormap
 (sb-mop:class-slots (class-of test-image)))

(print (array-dimensions (imago:image-pixels test-image)))

(setf *print-length* 10)

(defun array-to-list (array)
  (labels ((collect-row (dims ref-list)
             (cond ((car dims)
                    (loop for x below (car dims)
                          collect
                          (let ((ref (cons x ref-list)))
                            (if (cadr dims)
                                (collect-row (cdr dims) ref)
                                (apply #'aref array (reverse ref)))))))))
    (let ((dims (array-dimensions array)))
      (collect-row dims nil))))

(dec-to-hex 4285094947)


(defun dec-to-hex (dec)
  (write-to-string dec :base 16))

(defun hex-to-rgb (string)
  "Parse hexadecimal notation (eg ff0000 or f00 for red) into an RGB color."
  (let ((len (length string)))
    (flet ((parse (index)
             (parse-integer string
                            :start (* index 2)
                            :end   (* (1+ index) 2)
                            :radix 16)))
      (cons (parse 0)
            (cons (parse 1)
                  (cons (parse 2)
                        (if (= len 8) (list (parse 3)) nil)))))))

(equal (list 40 40 40) (list 40 40 40))

(hex-to-rgb (dec-to-hex 4280821800))
(parse-integer "28" :radix 16)

(aref )

(defun compare-image-data (image data)
  (let ((width) ;; data is the pixels from right to left and up to down in rows
      (new-image (imago:image-pixels (imago:read-image "/home/vagabond/test.png")))))
  (labels ((compare-row (height row)
             (loop :for y :from 0 :to height by 3  ;; every 3rd
                   :do (loop :for x :from row :to (+ row width) by 3
                             :do (if (not (equal (list (aref image x y)
                                                   (aref image (+ 1 x) (+ 1 y))
                                                   (aref image (+ 2 x) (+ 2 y)))
                                                (aref data (* x y))))
                                     (setf (aref data (* x y))))
                             )
                   )
             )))
  )


(defparameter test-image (time (imago:read-image
                           "/home/vagabond/test.png")))

(setf *print-length* 100)
(print (* 4 (* 3840 2160)))

(let ((x 0) (y 0)
    (wx 140) (wy 100))
  (let* ((xwin root-window)
         (window (xwin-to-window xwin))
         (test-image (imago:image-pixels test-image)))
    (unless window (setf window root-window))
    (let ((width (window-width window))
        (height (window-height window)))
      (let* ((image (xlib:get-image xwin :x x :y y :width width :height height))
             ;; (xlib:image-pixmap )
             (d (slot-value image 'xlib::data))
             (i (+ (* 4 wx) (* 4 wy width))))
        (print (list (aref d i)
                  (aref d (+ 1 i))
                  (aref d (+ 2 i))
                  (aref d (+ 3 i))))
        (print (reverse (hex-to-rgb (dec-to-hex (aref test-image wy wx)))))
        ;; (print (loop :for z :from 0 :to y
        ;;              :collect (hex-to-rgb (dec-to-hex (aref test-image z x)))))

        ;; (print (sb-mop:class-slots (class-of image)))
        ;; (print (array-dimensions d))
        ;; (copy-array d)
        ))))

    ;; (list (aref d 2) (aref d 1) (aref d 0))))


(setf *print-length* 100)
(let* ((image (time (xlib:get-image root-window :x 0 :y 0 :width 3840 :height 2160)))
       (d (slot-value image 'xlib::data)))
  (print (array-dimensions d))
  (list (aref d 2) (aref d 1) (aref d 0)))

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
