(in-package :stumpwm)
;; Set room state based on the contents of the screen

(ql:quickload :pngload)
(ql:quickload :opticl)
;;
(defun grab-values-from-jpeg (jpeg-file)
  (let ((data (opticl:read-jpeg-file jpeg-file)))
    (print data)))

(defun grab-values-from-image (image-data rx ry)
  ;; maybe this should focus on the sides...
  (let* ((data image-data))
    (loop for x from 0 to 2
          collect (aref data ry rx x))))

(defun grab-values-from-png (png-file)
  (grab-values-from-image (pngload:data (pngload:load-file (car (directory png-file))))))

(defun grab-values-from-jpeg (jpeg-file)
  (grab-values-from-image (opticl:read-jpeg-file jpeg-file)))

(defun image-to-hue (image-values)
  (mapcar #'(lambda (x)
              (list (apply #'rgb-to-hue x) 250))
          image-values))

(defvar *background-hash* (make-hash-table :test 'equal))

(defun get-background-color (x y)
  (or (gethash (list x y)
              *background-hash*)
     (setf (gethash (list x y)
                    *background-hash*)
           (grab-values-from-image *image* x y))))

(defun current-background ()
  (subseq (cl-ppcre:scan-to-strings "'[^\\s']+" (read-file-into-string "~/.fehbg")) 1))

(defvar *image* (opticl:read-jpeg-file (current-background)))
