(ql:quickload '(:zpng :png-read :cffi :clx))
(use-package :zpng)
(use-package :png-read)

(defun draw-mandelbrot (file)
  (bt:make-thread (lambda ()
                    (let* ((png (make-instance 'png
                                               :color-type :grayscale-alpha
                                               :width 1920
                                               :height 1080))
                           (image (data-array png))
                           (max 255))
                      (dotimes (y 200 (write-png png file))
                        (dotimes (x 200)
                          (let ((c (complex (- (/ x 100.0) 1.5) (- (/ y 100.0) 1.0)))
                                (z (complex 0.0 0.0))
                                (iteration 0))
                            (loop
                             (setf z (+ (* z z) c))
                             (incf iteration)
                             (cond ((< 4 (abs z))
                                    (setf (aref image y x 1) iteration)
                                    (return))
                                   ((= iteration max)
                                    (setf (aref image y x 1) 255)
                                    (return)))))))))
                  :name "png"))

;; (defmacro with-display (host (display screen root-window) &body body)
;; Look Here for color extraction |  http://www.roard.com/docs/cookbook/cbsu18.html
(defvar host "")
(defvar display (xlib:open-display host))
(defvar screen (first (xlib:display-roots display)))
(defparameter root-window (xlib:screen-root screen))

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

(set-light-state 5 255 (grab-from-screen (xlib:window-id root-window) 1 1) 150)

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
