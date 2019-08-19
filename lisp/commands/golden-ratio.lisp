(in-package #:stumpwm)

(defvar *ratio-set* nil)

(defvar golden-ratio-value 1.618)

(defun enable-ratio (value)
  (let ((resize-increment value)
      (frame (tile-group-current-frame (current-group))))
    ;; (setf *ratio-set* (cons frame *ratio-set*))
    (pushnew frame *ratio-set*)
    (resize-frame (current-group) frame
                  (round (* resize-increment golden-ratio-value)) :width)
    (resize-frame (current-group) frame
                  (round resize-increment) :height)))

(defun disable-ratio (value)
  (let* ((resize-increment (* -1 value))
         (frame (tile-group-current-frame (current-group))))
    (setf *ratio-set* (delete frame *ratio-set*))
    (resize-frame (current-group) frame
                  (round (* resize-increment golden-ratio-value)) :width)
    (resize-frame (current-group) frame
                  (round resize-increment) :height)))

(defun toggle-golden-ratio (value)
  (let ((frame (tile-group-current-frame (current-group))))
    (if (cl:null (cl:member frame *ratio-set*))
        (enable-ratio value)
        (disable-ratio value))))

(defcommand toggle-golden-ratio-command () ()
  (toggle-golden-ratio *golden-ratio-resize-increment*))


(defcommand move-with-ratio (dir) (:rest)
  (let ((frame (tile-group-current-frame (current-group))))
    (if (cl:null (cl:member frame *ratio-set*))
        (progn (move-focus (read-from-string (format nil ":~a" dir)))
               (toggle-golden-ratio *golden-ratio-resize-increment*))
        (progn
          (toggle-golden-ratio *golden-ratio-resize-increment*)
          (move-focus (read-from-string (format nil ":~a" dir)))
          (toggle-golden-ratio *golden-ratio-resize-increment*)
          (redisplay)))))

(defcommand toggle-golden-ratio-toplevel () ()
  (if (null *golden-ratio-toplevel*)
      (progn
        (stumpwm:define-key *top-map* (stumpwm:kbd "s-h") "move-with-ratio left")
        (stumpwm:define-key *top-map* (stumpwm:kbd "s-j") "move-with-ratio down")
        (stumpwm:define-key *top-map* (stumpwm:kbd "s-k") "move-with-ratio up")
        (stumpwm:define-key *top-map* (stumpwm:kbd "s-l") "move-with-ratio right")
        (setf *golden-ratio-toplevel* t))
      (progn
        (stumpwm:define-key *top-map* (stumpwm:kbd "s-j") "move-focus down")
        (stumpwm:define-key *top-map* (stumpwm:kbd "s-h") "move-focus left")
        (stumpwm:define-key *top-map* (stumpwm:kbd "s-k") "move-focus up")
        (stumpwm:define-key *top-map* (stumpwm:kbd "s-l") "move-focus right")
        (setf *golden-ratio-toplevel* nil))))
