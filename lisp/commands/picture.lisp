(in-package :stumpwm)

(ql:quickload '(:zpng :png-read :cffi :clx))
;; (use-package :zpng)
;; (use-package :png-read)

;; (root-window)

;; (let* ((frame (find-frame-by-coordinates posx posy)))
;;   (if (and frame
;;            (frame-window frame))
;;       (grab-rgb-from-window (window-xwin (frame-window frame))
;;                             (- posx (frame-x frame)) (- posy (frame-y frame)))
;;       (get-background-color posx posy)))

;; (let* ((image (xlib:get-image window :x x :y y :width 1 :height 1))
;;        (d (slot-value image 'xlib::data)))
;;   (print d)
;;   (rgb-to-hue (aref d 2) (aref d 1) (aref d 0)))

;; (defmacro with-display (host (display screen root-window) &body body)
;; Look Here for color extraction |  http://www.roard.com/docs/cookbook/cbsu18.html
(defvar host "")
(defvar display (xlib:open-display host))
(defvar screen (first (xlib:display-roots display)))
(defparameter root-window (xlib:screen-root screen))

(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defun take-screenshot (&optional (host ""))
  (with-display host (display screen root-window)
    (xlib:get-image root-window :x 0 :y 0 :width (xlib:screen-width screen) :height (xlib:screen-height screen))))

(defmacro with-default-display ((display &key (force nil)) &body body)
  `(let ((,display (xlib:open-default-display)))
     (unwind-protect
          (unwind-protect
               ,@body
            (when ,force
              (xlib:display-force-output ,display))))))

(defmacro with-default-display-force ((display) &body body)
  `(with-default-display (,display :force t) ,@body))

(defmacro with-default-screen ((screen) &body body)
  (let ((display (gensym)))
    `(with-default-display (,display)
       (let ((,screen (xlib:display-default-screen ,display)))
         ,@body))))

(defmacro with-default-window ((window) &body body)
  (let ((screen (gensym)))
    `(with-default-screen (,screen)
       (let ((,window (xlib:screen-root ,screen)))
         ,@body))))

(defun x-position ()
  (with-default-window (w)
    (xlib:query-pointer w)))

(defun x-size ()
  (with-default-screen (s)
    (values
     (xlib:screen-width s)
     (xlib:screen-height s))))


(defun raw-image->png (data width height)
  (let* ((png (make-instance 'zpng:png :width width :height height
                                       :color-type :truecolor-alpha
                                       :image-data data))
         (data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
        ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
        (rotatef (aref data y x 0) (aref data y x 2))
        (setf (aref data y x 3) 255)))
    png))

(multiple-value-bind (default-width default-height) (x-size))

  (defun reduce-resolution (png width height)
    (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha
                             :image-data )
    )

  (defun raw-image->png (data width height)
    (let* ((png (make-instance 'zpng:png :width width :height height
                                         :color-type :truecolor-alpha
                                         :image-data data))
           (data (zpng:data-array png)))
      (dotimes (y height)
        (dotimes (x width)
          ;; BGR -> RGB, ref code: https://goo.gl/slubfW
          ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
          (rotatef (aref data y x 0) (aref data y x 2))
          (setf (aref data y x 3) 255)))
      png))

(with-default-window (w)
  (xlib:get-raw-image w :x 0 :y 0 :width 1920 :height 1080 :format :z-pixmap))

  (with-default-window (w)
    (xlib:create-window )
    (xlib:map-window w))

(defun display-screenshot (image width height &optional (host ""))
  (with-default-window (root)
    (let* ((xwindow (xlib:create-window :parent root
                                       :x 0 :y 0 :width width :height height
                                       :event-mask (xlib:make-event-mask :exposure :key-press :button-press)))
           (gcontext (xlib:create-gcontext :drawable xwindow))
           (window (xwin-to-window xwindow)))
      (assign-window window (current-group))
      (reparent-window (current-screen) window)
      (xlib:map-window xwindow)
      (unwind-protect
           (handler-case
               (xlib:event-case (display :force-output-p t :discard-p t)
                 (:exposure ()
                            (xlib:put-image xwindow gcontext image :x 0 :y 0
                                                   :width width :height height))
                 (t () t))
             (end-of-file ()))
        (xlib:destroy-window xwindow)
        (kill-window window)))))

;; (xlib:drawable-p ())

;; (xlib:image-depth (raw-image->png (take-screenshot) 1920 1080))
;; (xlib:image-depth (raw-image->png (take-screenshot) 1920 1080))
;; (display-screenshot (take-screenshot) 1920 1080)

(let ((sc (take-screenshot)))
  (loop for x from 0 to (- (length sc) 1)
         collect (aref sc x)))

(time (xlib:image-depth (take-screenshot)))

  (defun x-snapshot (&key (x 0) (y 0)
                       (width default-width) (height default-height)
                       (delay 0)
                       path)
    "Return RGB data array (The dimensions correspond to the height, width,
and pixel components, see comments in x-snapsearch for more details),
or write to file (PNG only), depend on if you provide the path keyword"
    (sleep delay)
    (with-default-window (w)
      (let ((image
             (raw-image->png
              (xlib:get-raw-image w :x x :y y
                             :width width :height height
                             :format :z-pixmap)
              width height)))
        (if path
            (let* ((ext (pathname-type path))
                   (path (if ext path (concatenate 'string path ".png")))
                   (png? (or (null ext) (equal ext "png"))))
              (cond
                (png? (zpng:write-png image path))
                (t (error "Only PNG file is supported"))))
            (zpng:data-array image)))))

(time (x-snapshot :path "test.png"))
(display-screenshot (take-screenshot) 1920 1080)

(with-default-window (w)
  (let* ((window (xlib:create-window :parent w :x 0 :y 0 :width 1920 :height 1080)))
    (xlib:map-window window)
    (xlib:put-image window (xlib:create-gcontext :drawable window)
                  (xlib:get-image w :x 0 :y 0 :width 1920 :height 1080
                                    :format :z-pixmap)
                  :x 0 :y 0 :width 1920 :height 1080)))

(defun take-screenshot (&optional (host ""))
  (with-default-window (w)
    (xlib:get-raw-image w :x 0 :y 0
                          :width 1920 :height 1080
                          :format :z-pixmap)))


(defun take-screenshot ()
  (with-default-window (w)
    (xlib:get-image w :x 0 :y 0 :width 1920 :height 1080)))

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

(defvar screenshot-test (take-screenshot))

(time (save-screenshot-as-png (take-screenshot "") "test.png"))

(defun root-window ()
  (xlib:screen-root (car (xlib:display-roots display))))
;; (unwind-protect (progn ,@body)
;; (xlib:close-display ,display))))

;; (defparameter image (xlib:get-image root-window :x 5 :y 5 :width 5 :height 5))

(defun color-to-hue (px py)
  (let ((color (car (xlib:query-colors (xlib:window-colormap (root-window)) (list (+ px (* py (xlib:screen-width screen))))))))
    (xlib-to-hue (xlib:color-red color)
                 (xlib:color-green color)
                 (xlib:color-blue color))))

(defun %xlib-to-hue (red green blue)
  (print (list red green blue))
  (let ((max (max red green blue))
      (min (min red green blue)))
    (cond ((= red green blue)
           max)
          ((= red max)
           (* 60 (/ (- green blue) (- max min))))
          ((= green max)
           (* 60 (+ 2 (/ (- blue red) (- max min)))))
          ((= blue max)
           (* 60 (+ 4 (/ (- red green) (- max min))))))))

(defun dec-to-hex (dec)
  (write-to-string dec :base 16))

(defun grab-from-screen (win-id x y)
  (let ((rgb (parse-hex-rgb (run/s (format nil "grabc -w 0x~a -l ~a+~a" (dec-to-hex win-id) x y)))))
    (xlib-to-hue (rgb-red rgb) (rgb-green rgb) (rgb-blue rgb))))



(defun xlib-to-hue (red green blue)
  (let ((v (%xlib-to-hue red green blue)))
    (if (> v 0) (round (* 100 v)) (round (* 100 (+ 360 v))))))

;; Hex to RGB conversion

(deftype unit-real ()
  "Real number in [0,1]."
  '(real 0 1))

(defstruct (rgb (:constructor rgb (red green blue)))
  "RGB color."
  (red nil :type unit-real :read-only t)
  (green nil :type unit-real :read-only t)
  (blue nil :type unit-real :read-only t))

(defun parse-hex-rgb (string &key (start 0) end)
  "Parses a hexadecimal RGB(A) color string.  Returns a new RGB color value
and an alpha component if present."
  (let* ((length (length string))
         (end (or end length))
         (sub-length (- end start)))
    (cond
      ;; check for valid range, we need at least three and accept at most
      ;; nine characters
      ((and (<= #.(length "fff") sub-length)
            (<= sub-length #.(length "#ffffff00")))
       (when (char= (char string start) #\#)
         (incf start)
         (decf sub-length))
       (labels ((parse (string index offset)
                  (parse-integer string :start index :end (+ offset index)
                                        :radix 16))
                (short (string index)
                  (/ (parse string index 1) 15))
                (long (string index)
                  (/ (parse string index 2) 255)))
         ;; recognize possible combinations of alpha component and length
         ;; of the rest of the encoded color
         (multiple-value-bind (shortp alphap)
             (case sub-length
               (#.(length "fff") (values T NIL))
               (#.(length "fff0") (values T T))
               (#.(length "ffffff") (values NIL NIL))
               (#.(length "ffffff00") (values NIL T)))
           (if shortp
               (values
                (rgb
                 (short string start)
                 (short string (+ 1 start))
                 (short string (+ 2 start)))
                (and alphap (short string (+ 3 start))))
               (values
                (rgb
                 (long string start)
                 (long string (+ 2 start))
                 (long string (+ 4 start)))
                (and alphap (long string (+ 6 start))))))))
      (T
       (error "not enough or too many characters in indicated sequence: ~A"
              (subseq string start end))))))
