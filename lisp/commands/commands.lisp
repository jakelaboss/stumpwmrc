;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Command Definitions
;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Emacs Commands

;; (stumpwm:defcommand eshell () ()
;;                     (stumpwm::send-meta-key (stumpwm:current-screen) (stumpwm:meta SPC))
;;                     (stumpwm::send-meta-key (stumpwm:current-screen) (stumpwm:window-send-string "")))

(in-package :stumpwm)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Swank Server
;; (load "/home/vagabond/.emacs.d/elpa/slime-20170209.1240/swank-loader.lisp")
;; (swank-loader:init)

(ql:quickload :swank-client)

(defcommand swank () ()
  (swank:create-server :port 4008 :style swank:*communication-style*
                       :dont-close t)
  (echo-string (current-screen)
               "Starting swank on port 4006."))

(defcommand swank-lan () ()
  (swank:create-server :port 4008 :style swank:*communication-style* :interface "10.10.10.230"
                       :dont-close t)
  (echo-string (current-screen)
               "Starting swank on port 4006."))

(defparameter *desktop-swank* nil)

(defcommand desktop-connect () ()
  (defparameter *desktop-swank*
    (swank-client:slime-connect "10.10.10.225" 4006 (setf *desktop-swank* nil))))

(add-hook *selection-notify-hook* 'clipboard-history::save-clipboard-history)

(setf *print-length* 100)

(defparameter clipboard-history::*clipboard-poll-timeout* 30)

(import '(clipboard-history::*clipboard-poll-timeout*
          clipboard-history::*clipboard-timer*
          clipboard-history:start-clipboard-manager
          clipboard-history::poll-selection
        clipboard-history::*clipboard-history*
        clipboard-history::*clipboard-poll-timeout*))

(defun eval-current-selection ()
  (let ((timeout *clipboard-poll-timeout*))
    (when (setf *clipboard-poll-timeout* 0)
      (if (null *clipboard-timer*)
          (start-clipboard-manager))
      (progn
        (map nil #'poll-selection '(:clipboard :primary))
        (let ((result (eval (read-from-string (print (car *clipboard-history*))))))
          (setf *clipboard-poll-timeout* timeout)
          result)))))

(defcommand eval-from-clipboard () ()
  (unwind-protect (format nil "~a" (unwind-protect (eval-current-selection)))))

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Rewrite some commands
;;------------------------------------------------------------------------------------------------------------------------ ;;

(defun shell-command (command) "Run a shell command and display output to screen.
    This must be used in a functional side-effects-free style! If a program does not
    exit of its own accord, Stumpwm might hang!"
  (check-type command string)
  (echo-string (current-screen) (run-shell-command command t)))

(define-stumpwm-command "shell-command" ((command :string "sh: " :string))
  (check-type command string)
  (shell-command command))

(defun cat (&rest strings) "Concatenates strings, like the Unix command 'cat'.
    A shortcut for (concatenate 'string foo bar)."
  (apply 'concatenate 'string strings))


; --- Postgresql commands ----------------------------------------

(define-su-command pg-start "postgres" (concat "pg_ctl start -D " *pg-data*) :output t)

(define-su-command pg-status "postgres" (concat "pg_ct status -D " *pg-data*) :output t)

(define-su-command pg-stop "postgres" (concat "pg_ct stop -D " *pg-data*))


; --- Resize commands ----------------------------------------

;; Redefined - with `if`s for *useless-gaps-on*
(defun maximize-window (win)
  "Maximize the window."
  (multiple-value-bind (x y wx wy width height border stick)
      (geometry-hints win)

    (if *useless-gaps-on*
        (setf width (- width (* 2 *useless-gaps-size*))
              height (- height (* 2 *useless-gaps-size*))
              x (+ x *useless-gaps-size*)
              y (+ y *useless-gaps-size*)))

    (dformat 4 "maximize window ~a x: ~d y: ~d width: ~d height: ~d border: ~d stick: ~s~%" win x y width height border stick)
    ;; This is the only place a window's geometry should change
    (set-window-geometry win :x wx :y wy :width width :height height :border-width 0)
    (xlib:with-state ((window-parent win))
      ;; FIXME: updating the border doesn't need to be run everytime
      ;; the window is maximized, but only when the border style or
      ;; window type changes. The overhead is probably minimal,
      ;; though.
      (setf (xlib:drawable-x (window-parent win)) x
            (xlib:drawable-y (window-parent win)) y
            (xlib:drawable-border-width (window-parent win)) border)
      ;; the parent window should stick to the size of the window
      ;; unless it isn't being maximized to fill the frame.
      (if (or stick
              (find *window-border-style* '(:tight :none)))
          (setf (xlib:drawable-width (window-parent win)) (window-width win)
                (xlib:drawable-height (window-parent win)) (window-height win))
          (let ((frame (window-frame win)))
            (setf (xlib:drawable-width (window-parent win)) (- (frame-width frame)
                                                               (* 2 (xlib:drawable-border-width (window-parent win)))
                                                               (if *useless-gaps-on* (* 2 *useless-gaps-size*) 0))
                  (xlib:drawable-height (window-parent win)) (- (frame-display-height (window-group win) frame)
                                                                (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                (if *useless-gaps-on* (* 2 *useless-gaps-size*) 0)))))
      ;; update the "extents"
      (xlib:change-property (window-xwin win) :_NET_FRAME_EXTENTS
                            (list wx wy
                                  (- (xlib:drawable-width (window-parent win)) width wx)
                                  (- (xlib:drawable-height (window-parent win)) height wy))
                                              :cardinal 32))))

(defun reset-all-windows ()
  "Reset the size for all tiled windows"
  (let ((windows (mapcan (lambda (g)
                           (mapcar (lambda (w) w) (sort-windows g)))
                         (sort-groups (current-screen)))))
    (mapcar (lambda (w)
              (if (string= (class-name (class-of w)) "TILE-WINDOW")
                  (maximize-window w))) windows)))

;; --- Winlist with every open window ----------------------------------------
(defcommand all-windowlist (&optional (fmt *window-format*) window-list) (:rest)
  (let ((window-list (or window-list
                      (mapcar #'(lambda (x)
                                  (cons (window-name x)
                                        (cons (window-number x)
                                              (window-group x))))
                              (sort-windows-by-number
                               (screen-windows (current-screen)))))))
    (if (null window-list)
        (message "No Managed Windows")
        ;; (let ((window (select-window-from-menu window-list fmt)))
        (let* ((win-cons (select-from-menu (current-screen) window-list))
               (window
                 (select-window-by-number
                        (cadr win-cons)
                        (cddr win-cons))))
          (if (null win-cons) (throw 'error :abort)
              (progn
                (switch-to-group (cddr win-cons))
                (select-window-by-number (cadr win-cons) (cddr win-cons))))))))


;; --- kill command with safeguard ----------------------------------------

(defun safe-kill* (window)
  (cond ((cl-ppcre:scan "emacs" (window-name window))
         (window-send-string " qq"))
        ((cl-ppcre:scan "Firefox" (window-name window))
         (push-meta-key "C-w"))
        (t (kill-window window))))

(defcommand safe-kill () ()
  (safe-kill* (current-window)))

; --- Group Commands ----------------------------------------
;; Redefine group commands

(defun move-all-windows-to-group (&optional (group (next-group (current-group))))
  (move-windows-to-group (list-windows (current-group)) group))

(defcommand gnext-with-all () ()
  (move-all-windows-to-group (next-group (sort-groups (current-screen)))))

(defcommand gprev-with-all () ()
  (move-all-windows-to-group (car (nreverse (sort-groups (current-screen))))))

(defun group-forward (current list)
  "Switch to the next non-hidden-group in the list, if one
  exists. Returns the new group."
  (let ((ng (next-group current (non-hidden-groups list))))
    (when ng
      (switch-to-group ng)
      ng)))

(defun group-forward-with-window (current list)
  "Switch to the next group in the list, if one exists, and moves the
  current window of the current group to the new one."
  (let ((next (group-forward current list))
        (win (group-current-window current)))
    (when (and next win)
      (move-window-to-group win next)
      (really-raise-window win))))

(defcommand gnext-with-window () ()
  "Cycle to the next group in the group list, taking the current
window along."
  (group-forward-with-window (current-group)
                             (sort-groups (current-screen))))

(defcommand gprev-with-window () ()
  "Cycle to the previous group in the group list, taking the current
window along."
  (group-forward-with-window (current-group)
                             (reverse (sort-groups (current-screen)))))

(defcommand gaps () ()
   "Toggle the padding of tiled windows"
   (setf *useless-gaps-on* (null *useless-gaps-on*))
   ;; Following is pseudo code to use hooks
   ;; to do something like change border colors or size
   ;; (if *useless-gaps-on*
   ;;     (run-hook 'frame-gap-on)
   ;;     (run-hook 'frame-gap-off))
   (reset-all-windows))

;; Swank Commands

(defun group-forward-swank ()
  (let ((group-name (group-name
                   (next-group (current-group)
                               (sort-groups (current-screen))))))
    (swank-client:slime-eval-async
     `(if (equal (group-name
                  (next-group (current-group)
                              (sort-groups (current-screen))))
                 ,group-name)
          (switch-to-group
           (next-group (current-group)
                       (sort-groups (current-screen)))))
     *desktop-swank*)))

(defun group-backward-swank ()
  (let ((group-name (group-name
                   (next-group (current-group)
                               (nreverse (sort-groups (current-screen)))))))
    (swank-client:slime-eval-async
     `(if (equal (group-name
                  (next-group (current-group)
                              (nreverse (sort-groups (current-screen)))))
                 ,group-name)
          (switch-to-group
           (next-group (current-group)
                       (nreverse (sort-groups (current-screen))))))
     *desktop-swank*)))

(defcommand gnext-swank () ()
  (if *desktop-swank*
      (progn (group-forward-swank)
             (gnext))
      (gnext)))

(defcommand gprev-swank () ()
  (if *desktop-swank*
      (progn (group-backward-swank)
             (gprev))
      (gprev)))

(defcommand gnext-with-window-swank () ()
  (if *laptop-swank*
      (progn (group-forward-swank)
             (gnext-with-window))
      (gnext-with-window)))

(defcommand gprev-with-window-swank () ()
  (if *laptop-swank*
      (progn (group-backward-swank)
             (gprev-with-window))
      (gprev-with-window)))

(defcommand move-focus-up () ()
  (if (= (frame-y (tile-group-current-frame (current-group))) 0)
      (inferior-shell:run/s "xdotool key Up"))
      ;; (bordeaux-threads:make-thread (lambda ()
      ;;                                 (swank-client:slime-eval-async
      ;;                                  '(synergy-up) *desktop-swank*)))
      (move-focus :up))

(defcommand synergy-focus () ()
  (send-meta-key *current-screen* (kbd "s-Up")))

(defcommand synergy-up () ()
  (synergy-focus) nil)

(defun resize-dialogue (window)
  (let ((frame (window-frame window)))
    (progn
    (activate-fullscreen window)
    (deactivate-fullscreen window))))

(defcommand resize-popup () ()
  (if (equal (window-type (current-window)) :DIALOG)
      (resize-dialogue (current-window))))

; --- Brightness ----------------------------------------

(defvar *brightness-increment* nil)
(defvar *max-brightness* (read (open "/sys/class/backlight/intel_backlight/max_brightness")))
(defvar *keyboard-brightness-max* (read (open "/sys/class/leds/asus::kbd_backlight/max_brightness")))

(defcommand set-backlight (x) ((:number "Set backlight to %: "))
  "Set backlight to a specified number out of 100"
  (if (< 100 x) (error "Cannot be Above 100")
      (let ((percent (floor (* x *max-brightness*) 100)))
        (sudo-command (format nil "tee /sys/class/backlight/intel_backlight/brightness <<< ~a" percent)))))

(defcommand increase-brightness () ()
  (let* ((c1 (stumpwm:run-shell-command "cat /sys/class/backlight/intel_backlight/brightness" t))
         (n (parse-integer c1))
         (to-set (+ n 50)))
    (sudo-command (format nil "tee /sys/class/backlight/intel_backlight/brightness <<< ~a" to-set))))

(defcommand decrease-brightness () ()
  (let* ((c1 (stumpwm:run-shell-command "cat /sys/class/backlight/intel_backlight/brightness" t))
         (n (parse-integer c1))
         (to-set (- n 50)))
    (sudo-command (format nil "tee /sys/class/backlight/intel_backlight/brightness <<< ~a" to-set))))

(defcommand keyboard-backlight (x) ((:number "Set keyboard backlight to : "))
  ;; "Set backlight to a specified number out of 100"
  (if (< *keyboard-brightness-max* x) (error "number to big")
      (let ((numb x))
        (sudo-command (format nil "tee /sys/class/leds/asus::kbd_backlight/brightness <<< ~a" numb)))))

(defcommand reset-backlight () ()
  (run-shell-command "xset s activate"))

; --- Volume ----------------------------------------

(defcommand inc-volume () ()
  (print (cl-ppcre:scan-to-strings
          "\\d+%" (inferior-shell:run/s (format nil "amixer sset Master ~a+" 5)))))

(defcommand dec-volume () ()
  (print (cl-ppcre:scan-to-strings
          "\\d+%" (inferior-shell:run/s (format nil "amixer sset Master ~a-" 5)))))

(defcommand reset-audio () ()
  (run-shell-command "bash /home/vagabond/libraries/builds/zenbook-pro-ux501vw-sound-fix/fix-audio.sh"))

(defcommand mute-toggle () ()
  (let ((get (cl-ppcre:scan-to-strings "\\[on\\]"
                                     (inferior-shell:run/s "amixer sget Master"))))
    (if get (progn (inferior-shell:run/s "amixer sset Master mute") "Sound Muted")
        (progn (mapcar (lambda (x)
                         (inferior-shell:run/s (format nil "amixer sset ~a unmute " x)))
                       '("Master" "Speaker"))
               "Sound Unmuted"))))

;; ideas for restarting audio after suspend
;; $ pacmd list-cards
;; $ fuser -v /dev/snd/*

; ---- Xpra ----------------------------------------

(defcommand xpra (password) (:rest)
  (inferior-shell:run/s "xpra start ssh:dev-server:7 --start=firefox-developer-edition --auto-refresh-delay=.01 --encoding=h264"))

; --- Plover ----------------------------------------

;; (defcommand plover-toggle () ()
;;   (progn
;;     (loop for i in '("e" "r" "f" "v" "o" "l" "\" \"")
;;           do (run-shell-command (concatenate 'string "xdotool keydown " i)))
;;     (loop for i in '("e" "r" "f" "v" "o" "l" "\" \"")
;;           do (run-shell-command (concatenate 'string "xdotool keyup " i)))))

;; (defcommand plover-toggle () ()
;;   (run-shell-command "xdotool key e r f v o l " ))


; --- Xdotools----------------------------------------

(defun xdotool (keyseq)
  (inferior-shell:run/s (format nil "xdotool key ~a" keyseq)))

; --- Layout ----------------------------------------

(defun current-frame ()
  (window-frame (current-window)))

(defun window-width-inc (window)
  "Find out what is the correct step to change window width"
  (let ((h (window-normal-hints window)))
    (if h
       (or (xlib:wm-size-hints-width-inc h) 1)
       1)))

(defun window-height-inc (window)
  "Find out what is the correct step to change window height"
  (let ((w (window-normal-hints window)))
    (if w
        (or (xlib:wm-size-hints-height-inc w)
           1)
        1)))

(defun head-redisplay ()
  "Redisplay all windows in a head"
  (mapcar
   #'(lambda (window)
       (when window
         (with-slots (width height frame) window
           (set-window-geometry window
                                :width (- width (window-width-inc window))
                                :height (- height (window-height-inc window)))
           ;; make sure the first one goes through before sending the second
           (xlib:display-finish-output *display*)
           (set-window-geometry window
                                :width (+ width
                                          (* (window-width-inc window)
                                             (floor (- (frame-width frame) width)
                                                    (window-width-inc window))))
                                :height (+ height
                                           (* (window-height-inc window)
                                              (floor (- (frame-height frame) height)
                                                     (window-height-inc window)))))
           (maximize-window window))))
   (head-windows (current-group) (current-head))))

(defun center-frame (&optional (scale 1))
  "Toggle centering the frame in a head"
  (let* ((frames (head-frames (current-group) (current-head)))
         (head-width (head-width (current-head)))
         (centered-width (* 2 (round (* scale head-width) 4)))
         (margin (+ (head-x (current-head))
                    (/ (- (head-width (current-head))
                          centered-width) 2))))
    ;; now we need that number on both sides
    ;; Let's make the frame width half the max * golden ratio
    (if (> (length frames) 1) "Cannot center multiple frames"
        (progn
          (if (> (frame-x (car frames))
                 (head-x (current-head)))
              (setf (frame-x (car frames)) (head-x (current-head))
                    (frame-width (car frames)) head-width)
              (setf (frame-x (car frames)) margin
                    (frame-width (car frames)) centered-width))
          (head-redisplay)))))


(defvar *center-scale* 1)

(defcommand change-center-scale (scale) ((:number "Scale: "))
  (if (and (> scale 0) (< scale 2))
      (setf *center-scale* scale)))

(defcommand toggle-center-frame () ()
  (center-frame *center-scale*))


;; (defcommand interpret-swipe ()
;;     (if (cl-ppcre:scan "firefox"
;;                        (window-name (current-window)))
;;         (xdotool "ctrl+Tab") ;; so win
;;         (emacs-run "evil-win-right")
;;     )

; --- Export ----------------------------------------
;; TODO export useful commands/functions
