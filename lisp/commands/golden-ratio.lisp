(in-package #:stumpwm)

(defparameter *ratio-set* nil)

(defun enable-ratio (value)
  (let ((golden-ratio-value 1.618)
    (resize-increment value))
    (setf *ratio-set* t)
    (resize-frame (current-group)
                  (tile-group-current-frame (current-group))
                  (round (* resize-increment golden-ratio-value)) :width)
    (resize-frame (current-group)
                  (tile-group-current-frame (current-group))
                  (round resize-increment) :height)))

(defun disable-ratio (value)
  (let* ((golden-ratio-value 1.618)
         (resize-increment (* -1 value)))
    (setf *ratio-set* nil)
    (resize-frame (current-group)
                  (tile-group-current-frame (current-group))
                  (round (* resize-increment golden-ratio-value)) :width)
    (resize-frame (current-group)
                  (tile-group-current-frame (current-group))
                  (round resize-increment) :height)))

(defun toggle-golden-ratio (value)
  (if (equal *ratio-set* t)
      (disable-ratio value)
      (enable-ratio value)))

(defcommand toggle-golden-ratio-command () ()
  (toggle-golden-ratio *golden-ratio-resize-increment*))

