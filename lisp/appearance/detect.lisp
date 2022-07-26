(in-package :stumpwm)

(defpackage :eye-tracking
  (:use :cl :sosei ))

(in-package :eye-tracking)

;; let's use swig to generate an interface to this

(let ((process nil))
  (defun launch-process ()
    (when process (kill-process))
    (setf process (uiop:launch-program
                   ;; "/home/vagabond/.pyenv/versions/3.5.10/bin/python /home/vagabond/libraries/programming/interface/eye/GazeTracking/example.py"
                   "/home/vagabond/libraries/programming/interface/eye/eyeLike/build/bin/eyeLike"
                   :output :stream
                   :error :stream
                   :input :stream)))

  (defun kill-process ()
    (when process
      (uiop/launch-program::%posix-send-signal process 15)
      (setf process nil)))

  (defun ensure-process ()
    (unless (and process (uiop:process-alive-p process))
      (launch-process)))

  (defun alive-process ()
    (uiop:process-alive-p process))

  (defun raw-input ()
    (ensure-process)
    (uiop:process-info-output process)))


(defstruct eye pos left-corner right-corner)
(defvar left-eye (make-eye :pos '(61 80) :left-corner '(49.00 83.00) :right-corner '(74 81)))
(defvar right-eye (make-eye :pos '(61 80) :left-corner '(101.00 83.00) :right-corner '(130.00 81.00)))

(defmacro with-eye (eye &rest body)
  `(let ((pos (slot-value ,eye 'pos))
       (left-corner (slot-value ,eye 'left-corner))
       (right-corner (slot-value ,eye 'right-corner)))
     (declare (ignorable pos left-corner right-corner))
     ,@body))

(defun corner-x (eye)
  (with-eye eye
    (/ (+ (cadr left-corner) (cadr right-corner)) 2)))

;; x and y coordinates will be from 0 to 1
(defun eye-y (eye)
    ;; we have to make a choice....
    ;; the aspect ratio of the human eye is about 5:3
  (with-eye eye
    (let ((range (abs (- (car left-corner) (car right-corner))))
          (y (- (cadr pos) (corner-x eye))))
      (log (+ y (/ range 2)) range))))

(defun eye-x (eye)
  (with-eye eye
    (let ((range (abs (- (car left-corner) (car right-corner))))
          (x (+ 1 (- (car pos)
                     (min (car left-corner) (car right-corner))))))
      (/ x range))))

(defmacro corner-check (name eye)
  `(cond ,@(loop for dir in '("ll" "lr" "rl" "rr")
                 collect
                 `((equal ,(format nil "~a-corner:" dir) (subseq x 0 10))
                   (setf (slot-value ,eye ',corner)
                         (read-from-string (subseq x 9)))))))

(defun set-corner (eye side input)
  (if (or (equal side 'left) (equal side 'right))
      (setf (slot-value eye (symb side '-corner))
            (read-from-string (subseq input 10)))))

(defun eye-check (x)
  ;; TODO make this a macro, it's too ugly
  (cond ((equal "left:" (subseq x 0 5))
         (setf (slot-value left-eye 'pos)
               (read-from-string (subseq x 6))))
        ((equal "right:" (subseq x 0 6))
         (setf (slot-value right-eye 'pos)
               (read-from-string (subseq x 7))))
        ((equal "ll-corner:" (subseq x 0 10))
         (set-corner left-eye 'left x))
        ((equal "lr-corner:" (subseq x 0 10))
         (set-corner left-eye 'right x))
        ((equal "rl-corner:" (subseq x 0 10))
         (set-corner right-eye 'left x))
        ((equal "rr-corner:" (subseq x 0 10))
         (set-corner right-eye 'right x))))

(defun map-eye (x y)
  (let ((width 3840) (height 2400))
    (uiop:launch-program
     (format nil "xdotool mousemove ~a ~a"
             (round (* width (abs (- 1 x))))
             (round (* height (abs (- 1 y)))))
     :output nil :error nil :input nil)))

(defun print-eye (eye)
  (with-eye eye
    (print (list pos left-corner right-corner))))

(defun direction-check (eye)
  (with-eye eye
    ;; if the x values are less than 2, then we're in that direction
    (cond ((< (abs (- (car pos) (car left-corner))) 5) 'left) ;; if left eye and right corner
          ((< (abs (-  (car pos) (car right-corner))) 5) 'right)))) ;; if right eye and left corner

(defun direction (amount)
  (cond ((< (abs (- (car (slot-value left-eye 'pos))
                    (car (slot-value left-eye 'right-corner)))) amount) 'left) ;; if left eye and right corner
        ;; I might need to add 3 to the right side...
        ((< (abs (-  (car (slot-value right-eye 'pos))
                     (car (slot-value right-eye 'left-corner)))) (+ amount 5)) 'right))) ;; if right eye and left corner

(defun create-eye-loop ()
  (let ((stream (raw-input)) (i 0))
    (loop with stream = stream do
      ;; Characters are immediately available
      (progn
        (eye-check (read-line stream))
        (incf i)
        (cond ((= (mod i 100) 0)
               ;; (map-eye (eye-x) (eye-y))
               (print (direction 5))))))))

(create-eye-loop)

(defparameter *h-ratio* .5)
(defparameter *v-ratio* .5)

(defun eye-check-tracking (x)
  (let (h v)
    (cond ((equal "v:" (subseq x 0 2))
           (setf v (read-from-string (subseq x 3)))
           (if (and v (not (equal v 'none)))
               (setf *v-ratio* v)))
          ((equal "h:" (subseq x 0 2))
           (setf h (read-from-string (subseq x 3)))
           (if (and h (not (equal h 'none)))
               (setf *h-ratio* (- h .2)))))))

(defun map-eye (x y)
  (let ((width 3840) (height 2400))
    (uiop:launch-program
     (format nil "xdotool mousemove ~a ~a"
             (round (* width x))
             (round (* height y)))
     :output nil :error nil :input nil)))

(defun create-eye-loop ()
  (let ((stream (raw-input)) (i 0))
    (loop with stream = stream do
      ;; Characters are immediately available
      (progn
        (eye-check-tracking (read-line stream))
        (incf i)
        (if (= (mod i 3) 0)
            (map-eye *h-ratio* *v-ratio*))))))


(create-eye-loop)

(map-eye (abs (- 1 .48)) (abs (- 1 .3)))
