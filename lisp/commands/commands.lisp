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
(load "/home/arch/.emacs.d/elpa/slime-20161109.640/swank-loader.lisp")
(swank-loader:init)

;; For the not so lazy
(defcommand swank () ()
  (swank:create-server :port 4005 :style swank:*communication-style*
                       :dont-close t)
  (echo-string (current-screen)
               "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))
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

(defun update-all-mode-lines ()
  nil)

;; --- sudo command definitions  --------------------------------
(define-stumpwm-type :password (input prompt)
  (let ((history *input-history*)
        (arg (argument-pop input))
        (fn (symbol-function 'draw-input-bucket)))
    (unless arg
      (unwind-protect
           (setf (symbol-function 'draw-input-bucket)
                 (lambda (screen prompt input &optional errorp)
                   (let ((i (copy-structure input)))
                     (setf (input-line-string i)
                           (make-string (length (input-line-string i))
                                        :initial-element #\*))
                     (funcall fn screen prompt i)))
                 arg (read-one-line (current-screen) prompt))
        (setf (symbol-function 'draw-input-bucket) fn
              *input-history* history))
      arg)))

(defmacro define-sudo-command (name command &key output)
  (let ((cmd (gensym)))
    `(defcommand ,name (password) ((:password "sudo password: "))
       (let ((,cmd (concat "echo '" password "' | sudo -S " ,command)))
         ,(if output
              `(run-prog-collect-output *shell-program* "-c" ,cmd)
              `(run-prog *shell-program* :args (list "-c" ,cmd) :wait nil))))))

(defmacro define-su-command (name user command &key output)
  (let ((cmd (gensym)))
    `(defcommand ,name (password) ((:password "user password: "))
       (let ((,cmd (concat "echo '" password "' | su -c '" ,command "'" ,user)))
         ,(if output
              `(run-prog-collect-output *shell-program* "-c" ,cmd)
              `(run-prog *shell-program* :args (list "-c" ,cmd) :wait nil))))))

;; Postgresql commands

(define-su-command pg-start "postgres" (concat "pg_ctl start -D " *pg-data*) :output t)

(define-su-command pg-status "postgres" (concat "pg_ct status -D " *pg-data*) :output t)

(define-su-command pg-stop "postgres" (concat "pg_ct stop -D " *pg-data*))

;; (define-su-command pg- "postgres" (concat "pg_ct status -D " *pg-data*))

;; (define-sudo-command mount-media "mount -t ntfs-3g /dev/sda2 /home/arch/library/media/mnt/")

;; VPN ;;

(defun vpn (conf)
  (format nil (concatenate 'string
                           "openvpn /etc/openvpn/"
                           (concatenate 'string conf ".conf"))))

;; (define-sudo-command vpn (vpn "Brazil"))

; --- process management ----------------------------------------
(defun ps-exists (ps)
  (let ((f "ps -ef | grep ~S | grep -v -e grep -e stumpish | wc -l"))
    (< 0 (parse-integer (run-shell-command (format nil f ps) t)))))

(defun start-uniq-command-ps (command &key options (background t))
  (unless (ps-exists command)
    (run-shell-command
     (concat command " " options " " (when background "&")))))

(defun kill-ps-command (command)
  (format nil "kill -TERM `ps -ef | grep ~S | grep -v grep | awk '{print $2}'`"
          command))

(defun kill-ps (command)
  (run-shell-command (kill-ps-command command)))


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

;; Hope this works
;; Redefine group commands

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
