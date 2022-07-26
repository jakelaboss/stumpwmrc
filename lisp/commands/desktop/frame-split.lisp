(in-package :stumpwm)

(defun split-frame-in-head (group head frame how &optional (ratio 1/2))
  "Split the given frame into 2 frames. Return new frame number, if
it succeeded. NIL otherwise. RATIO is a fraction of the screen to
allocate to the new split window. If ratio is an integer then the
number of pixels will be used. This can be handy to setup the
desktop when starting."
  (check-type how (member :row :column))
    ;; don't create frames smaller than the minimum size
    (when (or (and (eq how :row)
                   (>= (frame-height frame) (* *min-frame-height* 2)))
              (and (eq how :column)
                   (>= (frame-width frame) (* *min-frame-width* 2))))
      (multiple-value-bind (f1 f2) (funcall (if (eq how :column)
                                                'split-frame-h
                                                'split-frame-v)
                                            group frame ratio)
        (setf (tile-group-frame-head group head)
              (if (atom (tile-group-frame-head group head))
                  (list f1 f2)
                  (funcall-on-node (tile-group-frame-head group head)
                                   (lambda (tree)
                                     (substitute (list f1 f2) frame tree))
                                   (lambda (tree)
                                     (unless (atom tree)
                                       (find frame tree))))))
        (migrate-frame-windows group frame f1)
        (choose-new-frame-window f2 group)
        (if (eq (tile-group-current-frame group)
                frame)
            (setf (tile-group-current-frame group) f1))
        (setf (tile-group-last-frame group) f2)
        (sync-frame-windows group f1)
        (sync-frame-windows group f2)
        ;; we also need to show the new window in the other frame
        (when (frame-window f2)
          (unhide-window (frame-window f2)))
        (frame-number f2))))

(defun clear-head (group head)
  "Delete all the frames and clear all windows from head"
  (let* ((screen (current-screen))
         (frame (copy-frame head)))
    (mapc (lambda (w)
            ;; windows in other frames disappear
            (hide-window w)
            (setf (window-frame w) frame))

          (remove-if (lambda (w) (typep w 'float-window))
                     (head-windows group head)))

    (setf (tile-group-frame-head group head) frame)
    (if (frame-window frame)
        (update-decoration (frame-window frame))
        (show-frame-indicator group))
    (sync-frame-windows group frame)))

;; Problem this doesn't actually solve the problem of a grid
(defun split-head (group head direction &optional (bi 4))
  (clear-head group head)
  (dotimes (x (- bi 1))
    (split-frame-in-head group head (lastcar (head-frames group head)) direction (/ 1 (- bi x)))))

;; currently done with columns first, then rows
(defun head-grid (group head &optional (bi 4))
  (clear-head group head)
  (dotimes (x (- bi 1))
    (split-frame-in-head group head (lastcar (head-frames group head)) :column (/ 1 (- bi x)))
    (dotimes (y (- bi 1))
      (split-frame-in-head group head (nth (+ y (* bi x)) (head-frames group head)) :row (/ 1 (- bi y)))))
  (dotimes (y (- bi 1))
    (split-frame-in-head group head (nth (+ y (* bi (- bi 1))) (head-frames group head)) :row (/ 1 (- bi y)))))

(defun head-grid (group head &optional (bi 4))
  (clear-head group head)
  (dotimes (x (- bi 1))
    (split-frame-in-head group head (lastcar (head-frames group head)) :row (/ 1 (- bi x)))
    (dotimes (y (- bi 1))
      (split-frame-in-head group head (nth (+ y (* bi x)) (head-frames group head)) :column (/ 1 (- bi y)))))
  (dotimes (y (- bi 1))
    (split-frame-in-head group head (nth (+ y (* bi (- bi 1))) (head-frames group head)) :column (/ 1 (- bi y)))))


;; (let* ((g (current-group)) (h (car (group-heads g))))
;;   (time (head-grid g h 5)))
