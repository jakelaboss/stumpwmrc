;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Keymap ;;
;;------------------------------------------------------------------------------------------------------------------------ ;;

(in-package :stumpwm)
;; Main;;

;; (defvar *main-map*
;;   (let ((m (stumpwm:make-sparse-keymap)))
;;     (stumpwm:define-key m (stumpwm:kbd "a") '*application-bindings*)
;;     (stumpwm:define-key m (stumpwm:kbd "g") '*group-bindings*)
;;     (stumpwm:define-key m (stumpwm:kbd "m") '*common-lisp-mode*)
;;     (stumpwm:define-key m (stumpwm:kbd "e") '*emacs-binding*)
;;     m
;;     ))
;; (stumpwm:define-key stumpwm:*top-map* (stumpwm:set-prefix-key "j") *main-map*)
;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Applications ;;
(defvar *application-bindings*
   (let ((m (stumpwm:make-sparse-keymap)))
     ;; (stumpwm:define-key m (stumpwm:kbd "e") "gnext")
     (stumpwm:define-key m (stumpwm:kbd "e") "exec sh -c 'emacsclient -c .'")
     ;; (stumpwm:define-key m (stumpwm:kbd "E") "exec sh -c \"emacs --eval \"(setq server-name \\\"work\\\")\" --daemon\"")
     (stumpwm:define-key m (stumpwm:kbd "E") "exec sh -c 'primusrun emacs'")
     (stumpwm:define-key m (stumpwm:kbd "a") "exec arandr")
     (stumpwm:define-key m (stumpwm:kbd "v") "exec pavucontrol-qt")
     (stumpwm:define-key m (stumpwm:kbd "u") "exec urxvt")
     (stumpwm:define-key m (stumpwm:kbd "s") "exec rofi -show ssh")
     (stumpwm:define-key m (stumpwm:kbd "S") "exec steam")
     (stumpwm:define-key m (stumpwm:kbd "F2") "exec conky")
     (stumpwm:define-key m (stumpwm:kbd "k") "exec keypass")
     (stumpwm:define-key m (stumpwm:kbd "p") "exec plover")
     (stumpwm:define-key m (stumpwm:kbd "r") "exec rofi -show drun")
     (stumpwm:define-key m (stumpwm:kbd "f") "exec firefox-developer-edition")
     (stumpwm:define-key m (stumpwm:kbd "F") "exec primusrun firefox-developer-edition")
     (stumpwm:define-key m (stumpwm:kbd "XF86AudioPlay") "exec spotify")
     (stumpwm:define-key m (stumpwm:kbd "g") "exec google-chrome-beta")
     m ; NOTE: this is important
     ))
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-a") '*application-bindings*)

(defcommand pavucontrol () ()
  (if (run-commands "exec pavucontrol-qt")
      (progn
        (select-window "Volume Control")
        (run-commands "redisplay"))))

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Group Configuration ;;
(defvar *group-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    ;; (stumpwm:define-key m (stumpwm:kbd "l") "gnext")
    (stumpwm:define-key m (stumpwm:kbd "l") "gnext-swank")
    (stumpwm:define-key m (stumpwm:kbd "s-l") "gnext-swank")
    ;; (stumpwm:define-key m (stumpwm:kbd "h") "gprev")
    (stumpwm:define-key m (stumpwm:kbd "s-h") "gprev-swank")
    (stumpwm:define-key m (stumpwm:kbd "h") "gprev-swank")
    (stumpwm:define-key m (stumpwm:kbd "L") "gnext-with-window")
    (stumpwm:define-key m (stumpwm:kbd "H") "gprev-with-window")
    (stumpwm:define-key m (stumpwm:kbd "m") "gmove")
    (stumpwm:define-key m (stumpwm:kbd "w") "grouplist")
    (stumpwm:define-key m (stumpwm:kbd "n") "gnew")
    (stumpwm:define-key m (stumpwm:kbd "N") "gnew-float")
    (stumpwm:define-key m (stumpwm:kbd "q") "gkill")
    (stumpwm:define-key m (stumpwm:kbd "r") "grename")
    (stumpwm:define-key m (stumpwm:kbd "k") "screen-next")
    (stumpwm:define-key m (stumpwm:kbd "j") "screen-prev")
    (stumpwm:define-key m (stumpwm:kbd "s") "snew")
    (stumpwm:define-key m (stumpwm:kbd "W") "screen-select")
    m ; NOTE: this is important
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-g") '*group-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Frame Configuration
(defvar *frame-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "r") "iresize")
    (stumpwm:define-key m (stumpwm:kbd "w") "windowlist")
    (stumpwm:define-key m (stumpwm:kbd "W") "all-windowlist")
    (stumpwm:define-key m (stumpwm:kbd "R") "title")
    (stumpwm:define-key m (stumpwm:kbd "b") "balance-frames")
    (stumpwm:define-key m (stumpwm:kbd "c") "center-frame")
    (stumpwm:define-key m (stumpwm:kbd "m") "mode-line")
    (stumpwm:define-key m (stumpwm:kbd "g") "gaps")
    (stumpwm:define-key m (stumpwm:kbd "f") "fullscreen")
    (stumpwm:define-key m (stumpwm:kbd "l") "set-backlight")
    (stumpwm:define-key m (stumpwm:kbd "L") "reset-backlight")
    (stumpwm:define-key m (stumpwm:kbd "n") "net-scan")
    m ; NOTE: this is important
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-f") '*frame-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Appearance Configuration

(defvar *wallpaper-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "o") "set-to-orange")
    (stumpwm:define-key m (stumpwm:kbd "g") "set-to-green")
    (stumpwm:define-key m (stumpwm:kbd "G") "set-to-grass")
    (stumpwm:define-key m (stumpwm:kbd "m") "set-to-mountains")
    (stumpwm:define-key m (stumpwm:kbd "p") "set-to-purple-mountains")
    (stumpwm:define-key m (stumpwm:kbd "l") "set-backlight")
    m ; NOTE: this is important
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-w") '*wallpaper-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Mode Commands ;;
;; TODO modes should be based on cur-frame

;; Common Lisp Mode
(defvar *common-lisp-mode-repl*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "c") "swank")
    m
    ))

(defvar *common-lisp-mode*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "s") '*common-lisp-mode-repl*)
    m
    ))

(stumpwm:define-key *common-lisp-mode* (stumpwm:kbd "e") '*common-lisp-mode-repl*)
(stumpwm:undefine-key *top-map* (stumpwm:kbd "s"))
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-m") '*common-lisp-mode*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Emacs Commands ;;
(defvar *emacs-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "e") "eshell")
    m
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-e") '*emacs-bindings*)

;; ------------------------------------------------------------------------------------------------------------------------ ;;
;; Audio Config ;;

(define-interactive-keymap (audio-control tile-group) ()
  ((stumpwm:kbd "-") "dec-volume")
  ((stumpwm:kbd "+") "inc-volume")
  ((stumpwm:kbd "r") "reset-audio"))

(define-key *top-map* (kbd "s-o") "audio-control")
;; ------------------------------------------------------------------------------------------------------------------------ ;;
;; Mode Line Commands
;; (defvar *mode-line-bindings*
;;   (let ((m (stumpwm:make-sparse-keymap)))
;;     (stumpwm:define-key m (stumpwm:kbd "R") "title")
;;     (stumpwm:define-key m (stumpwm:kbd "b") "balance-frames")
;;     m ; NOTE: this is important
;;     ))
;; (stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-") '*mode-line-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Database Commands ;;

(defvar *database-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "p") '*postgres-bindings*)
    m
    ))

(defvar *postgres-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "s") "pg-start")
    (stumpwm:define-key m (stumpwm:kbd "q") "pg-stop")
    m
    ))

;; (stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-d") '*database-bindings*)
;; (stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-d") '*database-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Quit Bindings ;;

(defvar *quit-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "q") "kill")
    m
    ))

(stumpwm:define-key *top-map* (stumpwm:kbd "s-q") '*quit-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Toggle Bindings

(defvar *toggle-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "g") "toggle-golden-ratio-command")
    (stumpwm:define-key m (stumpwm:kbd "p") "plover-toggle")
    (stumpwm:define-key m (stumpwm:kbd "r") "resize-popup")
    (stumpwm:define-key m (stumpwm:kbd "G") "toggle-golden-ratio-toplevel")
    (stumpwm:define-key m (stumpwm:kbd "h") "move-with-ratio left")
    (stumpwm:define-key m (stumpwm:kbd "j") "move-with-ratio down")
    (stumpwm:define-key m (stumpwm:kbd "k") "move-with-ratio up")
    (stumpwm:define-key m (stumpwm:kbd "l") "move-with-ratio right")
    m
    ))

(stumpwm:define-key *top-map* (stumpwm:kbd "s-t") '*toggle-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Golden Ratio Bindings
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

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Rofi Bindings
(defvar *rofi-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "r") "exec rofi -show run")
    (stumpwm:define-key m (stumpwm:kbd "w") "exec rofi -show window")
    (stumpwm:define-key m (stumpwm:kbd "s") "exec rofi -show ssh")
    m
    ))

(stumpwm:define-key *top-map* (stumpwm:kbd "s-u") '*rofi-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Setup Remote Swank Connection

;; (defvar *remote-bindings*
;;   (let ((m (stumpwm:make-sparse-keymap)))
;;     (stumpwm:define-key m (stumpwm:kbd "s-c") "")
;;     (stumpwm:define-key m (stumpwm:kbd "j") "move-focus down")
;;     (stumpwm:define-key m (stumpwm:kbd "h") "move-focus left")
;;     (stumpwm:define-key m (stumpwm:kbd "k") "move-focus up")
;;     (stumpwm:define-key m (stumpwm:kbd "l") "move-focus right")
;;     (stumpwm:define-key m (stumpwm:kbd "J") "move-window down")
;;     (stumpwm:define-key m (stumpwm:kbd "H") "move-window left")
;;     (stumpwm:define-key m (stumpwm:kbd "K") "move-window up")
;;     (stumpwm:define-key m (stumpwm:kbd "L") "move-window right")
;;     ;; move window
;;     (stumpwm:define-key m (stumpwm:kbd "J") "move-window down")
;;     (stumpwm:define-key m (stumpwm:kbd "H") "move-window left")
;;     (stumpwm:define-key m (stumpwm:kbd "K") "move-window up")
;;     (stumpwm:define-key m (stumpwm:kbd "L") "move-window right")
;;     ;; Splits WIndows and Frames
;;     (stumpwm:define-key m (stumpwm:kbd "v") "hsplit")
;;     (stumpwm:define-key m (stumpwm:kbd "s") "vsplit")
;;     (stumpwm:define-key m (stumpwm:kbd "r") "remove")
;;     (stumpwm:define-key m (stumpwm:kbd "q") "kill")
;;     (stumpwm:define-key m (stumpwm:kbd "-")"fclear")
;;     (stumpwm:define-key m (stumpwm:kbd "n") "pull-hidden-next")
;;     (stumpwm:define-key m (stumpwm:kbd "p") "pull-hidden-previous")
;;     m
;;     ))


;; this is a keymap that sends all kestrokes to a a port
;; Can either be the port 4005 for local stumpwm, 4006 for remote stumpwm
;; or 5900 for vnc server


;; (swank:create-server :port 4006 :style swank:*communication-style*
;;                      :dont-close t))


;; (defvar nas
;;   (swank-client:slime-connect "192.168.0.100" 4006))

;; (print nas)

;; (swank-client:slime-eval  nas)

;; (trivial-ssh:with-connection (conn "192.168.0.103"))
;; (trivial-ssh:pass arch "let'sgoexploring")

;; (swank-client:with-slime-connection (server "192.168.0.100" 4007) (+ 1 1))

(defvar *emacs-port* 4006)
(defvar *swank-client-port* 10000)

(defun start-swank-server-for-emacs (port)
  "Starts a Swank server thread, listening on PORT of the host's loopback
interface, to handle Emacs/Slime connection requests."
  (swank:create-server :port port :dont-close t))

(defun start-swank-server-for-swank-client (port)
  "Starts a Swank server thread, listening on PORT of the host's network
interface, to handle Swank Client connection requests."
  (let ((swank::*loopback-interface* (sb-unix:unix-gethostname)))
    (swank:create-server :port port :dont-close t)))

;; (defun swank-thread ()
;;   "Returns a thread that's acting as a Swank server."
;;   (dolist (thread (sb-thread:list-all-threads))
;;     (when (com.google.base:prefixp "Swank" (sb-thread:thread-name thread))
;;       (return thread))))

(defun wait-for-swank-thread ()
  "Wait for the Swank server thread to exit."
  (let ((swank-thread (swank-thread)))
    (when swank-thread
      (sb-thread:join-thread swank-thread))))

(defun main ()
  (setf swank:*configure-emacs-indentation* nil
        swank::*enable-event-history* nil
        swank:*log-events* t)
  (start-swank-server-for-emacs *emacs-port*)
  (start-swank-server-for-swank-client *swank-client-port*)
  (wait-for-swank-thread))

;; (main)

;; (swank-api:with-connection (swank-api:*emacs-connection*) ((swank:)

;; ((swank-api:send-to-remote-channel 4006    )))

;; (stumpwm:define-key *top-map* (stumpwmw) (window-send-string (format nil)"~a"))

;; (defmacro if-test (test then &optional else)
;;     `(cond (,test ,then)
;;            (,else)))


;; (defun if-test-defun (test then &optional else)
;;   (cond))

;; (cond (nil t)
;;       (t))

;; (if-test nil 'loo "FUCK YOU")


;; (swank-repl:listener-eval :port 4006 :style swank:*communication-style*
;;                      :dont-close t)

;; (let* ((server (usocket:socket-connect "127.0.0.1" 4006))
;;        (object (stream ))
;;   (usocket:socket-stream )}))

;; process to create a ssh link between local 4006 port and port on remote 4005 port

;; This will create a vnc-server on a remote server

;; (defmacro define-ssh-command (name ssh-arguments command)
;;   "Creates a ssh command to be run on"
;;   (let* ((nm name)
;;          (cmd command)
;;          (ssh ssh-arguments)
;;          (remote "192.168.0.100"))
;;     `(stumpwm:defcommand ,nm () ()
;;        (let* ((password *password*)
;;               (if (null ,port)
;;                   (stumpwm:run-shell-command (concat "sshpass -p \"" password
;;                                                      "\" ssh -o StrictHostKeyChecking=no "
;;                                                      ,ssh ,remote " '" ,cmd "'"))))
;;          ))))

;; (stumpwm:defcommand swank-connect () ()
;;   (let* ((password "let'sgoexploring"))
;;     (inferior-shell:run/s (format nil "sshpass -p \"~a\" ssh -L 4006:127.0.0.1:4005 192.168.0.100" password)))))

;; (let* ((password "let'sgoexploring"))
;;   (inferior-shell:run/ss (format nil "sshpass -p \"~a\" ssh -L 4006:127.0.0.1:4005 192.168.0.100" password)))

;; "sshpass -p \"let'sgoexploring\" ssh -L 4006:127.0.0.1:4005 192.168.0.100"

;;      "-t -L 5900:localhost:5900 "

;; ssh -L4006:127.0.0.1:4005 192.168.0.100

;; TODO change from run-shell-command to usocket and inferior-shell
(stumpwm:defcommand vnc-server () ()
  (let* ((password "let'sgoexploring")
         (remote "192.168.0.103")
         (display "1"))
    (stumpwm:run-shell-command (concat "sshpass -p \"" password
                                       "\" ssh -o StrictHostKeyChecking=no -t -L 5900:localhost:5900 "
                                       remote
                                       " 'x11vnc "
                                       "x11vnc -noxdamage -many -display :"
                                       display
                                       "-auth /home/arch/.Xauthority'"))))


(stumpwm:defcommand link-desktop () ()
  (let* ((password "let'sgoexploring")
         (remote "192.168.0.103"))
    (stumpwm:run-shell-command (concat "sshpass -p \"" password
                                       "\" ssh -o StrictHostKeyChecking=no -L 5900:localhost:5900 "
                                       remote))))


;; (defmacro create-link (name remote local-port remote-port)
;; `(stumpwm:defcommand ,name () ()
;;   (let* ((password "let'sgoexploring")
;;          (remote ,remote))
;;     (stumpwm:run-shell-command (concat "sshpass -p \"" password
;;                                        "\" ssh -o StrictHostKeyChecking=no -L 5900:localhost:5900 "
;;                                        remote))))


(stumpwm:defcommand vnc-viewer () ()
  (stumpwm:run-shell-command "vncviewer localhost:0"))

;; (print (concat "sshpass -p \"" "let'sgoexploring"
               ;; "\" ssh -o StrictHostKeyChecking=no -t -L 5900:localhost:5900 "
;;                "192.168.0.103"
;;                " 'x11vnc -noxdamage -many -display :0 -auth /home/arch/.Xauthority'"))


;; ;; "modprobe acpi_call"
;; ;; "/usr/share/acpi_call/examples/turn_off_gpu.sh"
