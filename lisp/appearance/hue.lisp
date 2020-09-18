(in-package :stumpwm)
(ql:quickload :cl-hue)

(import 'inferior-shell:run/s)
(import 'cl-ppcre:scan-to-strings)

;; Room Config
(defparameter nebula-values '((49576 204) (48379 234) (47167 118)))
(defparameter forest-values '((34602 135) (40750 128) (23597 68)))
(defparameter sunset-values '((7417 136) (5288 191) (1480 234)))

(defun nmap (&optional portscan?)
  (let ((ps (if portscan? "-sn" ""))
        (lan (scan-to-strings "(?! inet )(\\d+.\\d+.\\d+)"
                              (scan-to-strings "wlp3s0(.*\\n*)*" (run/s "ip addr")))))
    (run/s (format nil "nmap ~a.0/24 ~a" lan ps))))

(defun hue-bridge-address ()
  (scan-to-strings "(?<=Philips-hue \\()(\\d+\\.\\d+\\.\\d+\\.\\d+)" (nmap t)))

;; (defparameter username (cl-hue:create-user (hue-bridge-address) "cl-hue"))
(defparameter username (sosei:pread* "keys/hue-light-username"))

(defparameter *bridge* nil)

(defun bridge ()
  (if *bridge* *bridge*
      (let ((br (cl-hue:make-bridge (hue-bridge-address) username)))
        (defparameter *bridge* br)
        br)))


(defun get-light-state (light)
  (list (slot-value (cl-hue:get-light (bridge) light) 'cl-hue::hue)
        (slot-value (cl-hue:get-light (bridge) light) 'cl-hue::saturation)))

(defun get-room-state ()
  (loop for x from 5 to 7 ;; 5, 6, and 7 are the light numbers for this room
        collect (get-light-state x)))

(defun set-light-state (light brightness hue sat)
  (cl-hue:set-light-state-by-number (bridge) light :on t
                                                 :brightness brightness :hue hue
                                                 :saturation sat :effect "none"))

(defun set-room-state (values)
  (if *bridge*
      (let ((n 0))
        (loop for x from 5 to 7
              do (set-light-state x 255 (car (nth n values)) (cadr (nth n values)))
                 (incf n)))))

;; Example:
;; (set-room-state nebula-values)

;; Set room state based on the contents of the screen

(ql:quickload :pngload)
(ql:quickload :opticl)
;;
(defun grab-values-from-jpeg (jpeg-file)
  (let ((data (opticl:read-jpeg-file jpeg-file)))
    (print data)))


(defun grab-values-from-image (image-data)
  ;; maybe this should focus on the sides...
  (let* ((data image-data)
         (rx (random (array-dimension data 0)))
         (ry (random (array-dimension data 1))))
    (list
     (loop for x from 0 to 2
           collect (aref data rx 0 x))
     (loop for x from 0 to 2
           collect (aref data 0 ry x))
     (loop for x from 0 to 2
           collect (aref data rx ry x)))))

(defun grab-values-from-png (png-file)
  (grab-values-from-image (pngload:data (pngload:load-file (car (directory png-file))))))

(defun grab-values-from-jpeg (jpeg-file)
  (grab-values-from-image (opticl:read-jpeg-file jpeg-file)))

(defun %rgb-to-hue (rgb-value-list)
  (multiple-value-bind (red green blue)
      (values-list (mapcar #'(lambda (x) (/ x 255))
                           rgb-value-list))
    (let ((max (max red green blue))
          (min (min red green blue)))
      (cond ((= red green blue)
             max)
            ((= red max)
             (* 60 (/ (- green blue) (- max min))))
            ((= green max)
             (* 60 (+ 2 (/ (- blue red) (- max min)))))
            ((= blue max)
             (* 60 (+ 4 (/ (- red green) (- max min)))))))))

(defun rgb-to-hue (rgb-value-list)
  (let ((v (%rgb-to-hue rgb-value-list)))
    (if (> v 0) (round (* 100 v)) (round (* 100 (+ 360 v))))))

(defun image-to-hue (image-values)
  (mapcar #'(lambda (x)
              (list (rgb-to-hue x) 250))
          image-values))

;; Usage
;; (defvar *forest-values* (grab-values-from-jpeg
                         ;; (concat *green* "7162209cbe40aeeba705870210e5eb7d.jpg")))
;; (set-room-state (image-to-hue *forest-values*))
