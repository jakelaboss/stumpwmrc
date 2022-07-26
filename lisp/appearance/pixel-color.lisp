(in-package :stumpwm)
;; Grabs the color of a pixel a window (or the screen) and sets the hue color based on it

(defvar host "")

(defvar display (xlib:open-display host))
(defvar screen (first (xlib:display-roots display)))
(defvar root-window (xlib:screen-root screen))

(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))


(defun %rgb-to-hue (red green blue)
  (multiple-value-bind (red green blue)
      (values-list (mapcar #'(lambda (x) (/ x 255))
                           (list red green blue)))
    (let* ((max (max red green blue))
        (min (min red green blue))
           (chroma (- max min))
           (brightness (+ (* red .21) (* green .72) (* blue .07)))
           (res))
      (cond ((= red green blue)
             (setf res (* 60 max)))
            ((= red max)
             (setf res (* 60 (/ (- green blue) (- max min)))))
            ((= green max)
             (setf res (* 60 (+ 2 (/ (- blue red) (- max min))))))
            ((= blue max)
             (setf res (* 60 (+ 4 (/ (- red green) (- max min)))))))
      (list res chroma brightness))))

(defun rgb-to-xy (red green blue)
  (labels ((convert (val scale)
             (if (> val .04045)
                 (expt (/ (+ (+ val scale) .055) (+ 1.0 0.055)) 2.4)
                 (/ val 12.92))))
    (let* ((red (convert (/ red 255) .005))
           (green (convert (/ green 255) 0))
           (blue (convert (/ blue 255) 0))
           (X (+ (* red 0.649926) (* green 0.103455) (* blue 0.197109)))
           (Y (+ (* red 0.234327) (* green 0.743075) (* blue 0.022598)))
           (Z (+ (* red 0.0000000) (* green 0.053077) (* blue 1.035763))))
      ;; we can also use Y as brightness if we'd like
      (if (zerop (+ X Y Z)) (list 0 0)
          (list (/ X (+ X Y Z)) (/ Y (+ X Y Z)) Y)))))

(defun rgb-to-hue (red green blue)
  (let* ((res (%rgb-to-hue red green blue))
         (v (car res))
         (c (nth 1 res))
         (b (nth 2 res))
         (scale (/ 655.35 360))) ;; our system is out of 65500 colors
    (list (round (* scale (if (>= v 0) (* 100 v) (* 100 (+ 360 v))))) c b)))

(defun screen-px-to-hue (window px py)
  (let ((color (car (xlib:query-colors
                   (xlib:window-colormap window)
                   (list (+ px (* py (xlib:screen-width screen))))))))
    (rgb-to-hue (xlib:color-red color)
                 (xlib:color-green color)
                 (xlib:color-blue color))))

(defun grab-rgb-from-window (window x y)
  (let ((window window))
    (unless window (setf window root-window))
    (let* ((image (xlib:get-image window :x x :y y :width 1 :height 1))
           (d (slot-value image 'xlib::data)))
      (list (aref d 2) (aref d 1) (aref d 0)))))

(defun grab-from-screen (window x y)
  (unless window (setf window root-window))
  (let* ((image (xlib:get-image window :x x :y y :width 1 :height 1))
         (d (slot-value image 'xlib::data)))
    (print d)
    (rgb-to-hue (aref d 2) (aref d 1) (aref d 0))))

(defun dec-to-hex (dec)
  (write-to-string dec :base 16))

;; (defun grab-from-screen (win-id x y)
;;   "Using grabc"
;;   (let ((rgb (print (parse-hex-rgb (run/s (format nil "grabc -w 0x~a -l ~a+~a" (dec-to-hex win-id) x y))))))
;;     (xlib-to-hue (rgb-red rgb) (rgb-green rgb) (rgb-blue rgb))))

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
