(in-package :stumpwm)
(ql:quickload :cl-hue)

(import 'inferior-shell:run/s)
(import 'cl-ppcre:scan-to-strings)

;; Room Config
(defparameter nebula-values '((48379 234) (47167 118)  (49576 204)))
(defparameter forest-values '((34602 135) (40750 128) (23597 68)))
(defparameter sunset-values '((7417 136) (5288 191) (1480 234)))
(defparameter reading-values '((8597 121) (8597 121) (8597 121) (8595 121)))
(defparameter twilight-values '((4325 120) (52796 89) (49460 150)))

(defun nmap (&optional portscan?)
  "scans the current subnet"
  (let ((ps (if portscan? "-sn" ""))
      (lan (scan-to-strings "(?! inet )(\\d+\\.\\d+\\.\\d+)"
                            (scan-to-strings
                             (format nil "~a(.*\\n*)*" *device*)
                             (run/s "ip addr")))))
    (run/s (format nil "nmap ~a.0/24 ~a" lan ps))))

(defun hue-bridge-address ()
  "grabs the hue bridge address if it's found on the local subnet"
  (scan-to-strings "(?<=Philips-hue \\()(\\d+\\.\\d+\\.\\d+\\.\\d+)" (nmap t)))

(defun create-hue-user ()
  (defparameter username (cl-hue:create-user (hue-bridge-address) "cl-hue")))

;; for the apartment
(defparameter username (sosei:pread* "keys/hue-light-username"))

;; for garden st
;; (defparameter username (sosei:pread* "keys/hue-light-username-home"))

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
  "Returns a hash of all the active lights"
  (let ((hs (make-hash-table)))
    (loop for x from 0 to 20
          do (let ((vx (handler-case
                         (get-light-state x)
                       (error (c)
                         (declare (ignorable c))
                         nil))))
               (if vx (setf (gethash x hs) vx))))
    hs))

(defparameter *room* nil)
(defparameter *living-room* nil)
(defvar *bedroom* nil) ;; just the first 3

(defun define-rooms ()
  (defparameter *room* (get-room-state))
  (defparameter *living-room* (alist-hash-table (subseq (hash-table-alist *room*) 3)))
  (defparameter *bedroom* (alist-hash-table (subseq (hash-table-alist *room*) 0 3))) ;; just the first 3
  )

(defun set-light-hue (light brightness hue sat)
  (cl-hue:set-light-state-by-number
   (bridge) light :on t
   :brightness brightness :hue hue
   :saturation sat :effect "none"))

(defun set-light-xy (light brightness xy sat)
  (cl-hue:set-light-state-by-number
   (bridge) light :on t
                  :brightness brightness
                  :saturation sat :effect "none"
                  :xy xy))

(defun turn-light-off (light)
  (cl-hue:set-light-state-by-number
   (bridge) light :on nil))

(defun turn-light-on (light)
  (cl-hue:set-light-state-by-number
   (bridge) light :on t))

(defun turn-light-down (light &optional amount)
  (let* ((l (cl-hue:get-light (bridge) light))
         (amount (or amount 10))
         (brightness (- (slot-value l 'cl-hue::brightness) amount)))
    (unless (> brightness 0) (setf brightness 1))
    (cl-hue:set-light-state-by-number
     (bridge) light :on t
     :brightness brightness)))

(defun turn-light-up (light &optional amount)
  (let* ((l (cl-hue:get-light (bridge) light))
         (amount (or amount 10))
         (brightness (+ (slot-value l 'cl-hue::brightness) amount)))
    (unless (< brightness 256) (setf brightness 255))
    (cl-hue:set-light-state-by-number
     (bridge) light :on t
     :brightness brightness)))

(defun set-room-state (values)
  (if *bridge*
      (let ((n 0))
        (loop for x in (hash-table-keys *bedroom*)
              do (set-light-hue x 255 (car (nth n values)) (cadr (nth n values)))
                 (if (nth (+ 1 n) values)
                     (incf n)
                     (setf n 0))))))

(defun set-state-of-room (room values)
  (if *bridge*
      (let ((n 0))
        (loop for x in (hash-table-keys room)
              do (set-light-hue x 255 (car (nth n values)) (cadr (nth n values)))
                 (if (nth (+ 1 n) values)
                     (incf n)
                     (setf n 0))))))

(defun extract-light-state ()
  (hash-table-alist *room*))

;; Example:

(defun set-light-from-rgb (light rgb)
  (let ((xy (rgb-to-xy rgb))
      (sat 150))
    ;; (unless (> xy 20) (setf sat 0))
    (set-light-xy light 255 xy sat)))

(defun magick-grab-color-from-screen (xoffset yoffset)
  "Will return nil if all values are 0"
  (let* ((output (run/s (format nil "import -silent -window root -crop 1x1+~a+~a -depth 8 txt:-" xoffset yoffset)))
         (scan (scan-to-strings "(?<=srgb\\()\\d+,\\d+,\\d+" output)))
    (if scan (mapcar #'parse-integer (cl-ppcre:split "," (scan-to-strings "(?<=srgb\\()\\d+,\\d+,\\d+" output)))
        '(0 0 0))))
;; takes around .7 seconds, far to much to run multiple times in a second

(defun hue-win-id ()
  (if (current-window) (window-id (current-window))
      (xlib:window-id root-window)))

(defun hue-window ()
  "goes off the root window if there is no current window"
  (if (current-window) (window-xwin (current-window))
      root-window))

(defun find-frame-by-coordinates (posx posy)
  "returns the frame at position x,y; or nil if it can't find one"
  (let* ((frames (group-frames (current-group))))
    (car (remove-if-not #'(lambda (f)
                            (and (and (< (frame-x f) posx) ;; check we're in the right column
                                  (> (+ (frame-x f) (frame-width f)) posx))
                               (and (< (frame-y f) posy) ;; check we're in the right row
                                  (> (+ (frame-y f) (frame-height f)) posy))))
                        frames))))

(defun color-by-coordinates (posx posy)
  (let* ((frame (find-frame-by-coordinates posx posy)))
    (if (and frame
           (frame-window frame))
        (grab-rgb-from-window (window-xwin (frame-window frame))
                              (- posx (frame-x frame)) (- posy (frame-y frame)))
        (get-background-color posx posy))))

(defun hue-window ()
  "goes off the root window if there is no current window"
  (if root-window root-window
   (if (current-window) (window-xwin (current-window)))))

(defun hue-window-width ()
  (if (current-window) (window-width (current-window))
      (xlib:screen-width screen)))

(defun hue-window-height ()
  (if (current-window) (window-height (current-window))
      (xlib:screen-height screen)))

;; Calculating the average of rgb colors is a bit counter intuitive. It's not a straight average.
;; but the square root of the average sum of the square of each value.
(defun calculate-average-colors (color-list)
  "Take the square root of the sum of the square of each value"
  (loop for x from 0 to 2
        collect (round (sqrt (mean (mapcar #'(lambda (l)
                                            (expt (nth x l) 2))
                                        color-list))))))

(defun get-average-color-cw ()
  "Grabs the average color of the current window"
  (let* ((tl (color-by-coordinates 100 100)) ;; top left
         (bl (color-by-coordinates 100 ;; bottom left
                                   (- (hue-window-height) 100)))
         (c (color-by-coordinates ;; center
                                  (round (/ (hue-window-width) 2))
                                  (round (/ (hue-window-height) 2))))
         (tr (color-by-coordinates ;;top right
                                   (- (hue-window-width) 100) 100))
         (br (color-by-coordinates ;;bottom right
                                   (- (hue-window-width) 100)
                                   (- (hue-window-height) 100)))
         (rgb (calculate-average-colors (list tl bl c tr br))))
    (apply #'rgb-to-xy rgb)))

(defun grab-color-range (x y scale)
  "grabs the the color around an x and y area"
  (let ((colors (list (list x y) (list (- x scale) y) (list (+ x scale) y)
                 (list x (- y scale)) (list x (+ y scale)) (list (- x scale) (- y scale))
                 (list (+ x scale) (- y scale)) (list (+ x scale) (+ y scale)))))
    (calculate-average-colors
     (mapcar  #'(lambda (x)
                  (color-by-coordinates (car x) (cadr x)))
              colors))))

(defun get-average-color-hue ()
  "grabs the average color as an x y and brightness of the current head/screen"
  (let* ((tl (grab-color-range 100 100 5)) ;; top left
         (bl (grab-color-range 100 ;; bottom left
                                   (- (head-height (current-head)) 100) 5))
         (c (grab-color-range ;; center
                                  (round (/ (head-width (current-head)) 2))
                                  (round (/ (head-height (current-head)) 2)) 5))
         (tr (grab-color-range ;;top right
                                   (- (head-width (current-head)) 100) 100 5))
         (br (grab-color-range ;;bottom right
                                   (- (head-width (current-head)) 100)
                                   (- (head-height (current-head)) 100) 5))
         (rgb (calculate-average-colors (list tl bl c tr br))))
    (apply #'rgb-to-hue rgb)))

;; (time (get-average-color-cw))
;; takes .02 seconds, and runs color-by-coordinates 5 times
(defparameter *hue-loop* t)

(setf *hue-pan*
      (sosei:plambda ()
                     ((last))
                     (loop while *hue-loop*
                           do (let* ((av (get-average-color-hue))
                                     (color (car av))
                                     (sat (round (* 255 (* 1.5 (nth 1 av)))))
                                     (brightness (round (* 255 (* 1.2 (nth 2 av))))))
                                (print av)
                                (unless (equal av last)
                                  (loop for x in (hash-table-keys *living-room*)
                                        do (set-light-hue x (if (> brightness 255) 255 brightness)
                                                          color (if (> sat 255) 255 sat))))
                                (setf last av)
                                (sleep (* .2 (hash-table-count *living-room*)))))))

(defun light-sync-on ()
  (defparameter *hue-thread*
    (bt:make-thread *hue-pan*
                    :name "Hue Loop")))

(defun light-sync-off ()
  (let ((thread (find-if #'(lambda (x) (cl-ppcre:scan "Hue" (bt:thread-name x)))
                       (sb-thread:list-all-threads))))
    (if thread (bt:destroy-thread thread))))

(defun turn-all-lights-off ()
  (mapcar #'turn-light-off
          (hash-table-keys *room*)))

(defun turn-living-room-off ()
  (mapcar #'turn-light-off
          (hash-table-keys *living-room*)))

(defun turn-living-room-down ()
  (mapcar #'(lambda (x) (turn-light-down x 50))
          (hash-table-keys *living-room*)))

(defun turn-living-room-up ()
  (mapcar #'(lambda (x) (turn-light-up x 50))
          (hash-table-keys *living-room*)))

(defun turn-living-room-on ()
  (mapcar #'turn-light-on
          (hash-table-keys *living-room*)))

(defun turn-bedroom-off ()
  (mapcar #'turn-light-off
          (hash-table-keys *bedroom*)))

(defun turn-bedroom-on ()
  (set-state-of-room *bedroom* nebula-values))

(defun turn-bedroom-down ()
  (mapcar #'(lambda (x) (turn-light-down x 50))
          (hash-table-keys *bedroom*)))

(defun turn-bedroom-up ()
  (mapcar #'(lambda (x) (turn-light-up x 50))
          (hash-table-keys *bedroom*)))
