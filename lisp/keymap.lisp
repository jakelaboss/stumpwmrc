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
     (stumpwm:define-key m (stumpwm:kbd "K") "exec encryptr")
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
;;------------------------------------------------------------------------------------------------------------------------ ;;
(defvar *group-bindings*
  (let ((m (make-sparse-keymap)))
    ;; (define-key m (kbd "l") "gnext")
    (define-key m (kbd "l") "gnext-swank")
    (define-key m (kbd "s-l") "gnext-swank")
    ;; (define-key m (kbd "h") "gprev")
    (define-key m (kbd "s-h") "gprev-swank")
    (define-key m (kbd "h") "gprev-swank")
    (define-key m (kbd "L") "gnext-with-window")
    (define-key m (kbd "H") "gprev-with-window")
    (define-key m (kbd "m") "gmove")
    (define-key m (kbd "w") "grouplist")
    (define-key m (kbd "W") "grouplist-all")
    (define-key m (kbd "n") "gnew")
    (define-key m (kbd "N") "gnew-float")
    (define-key m (kbd "q") "gkill")
    (define-key m (kbd "r") "grename")
    (define-key m (kbd "R") "ws-rename")
    (define-key m (kbd "k") "ws-next")
    (define-key m (kbd "s-k") "ws-next")
    (define-key m (kbd "K") "ws-next-with-window")
    (define-key m (kbd "j") "ws-prev")
    (define-key m (kbd "s-j") "ws-prev")
    (define-key m (kbd "J") "ws-prev-with-window")
    (define-key m (kbd "s") "ws-new")
    (define-key m (kbd "S") "ws-select")
    m ; NOTE: this is important
    ))

(define-key *top-map* (kbd "s-g") '*group-bindings*)

(define-interactive-keymap (group-interactive-keymap tile-group) ()
  ((kbd "l") "gnext-swank")
  ((kbd "s-l") "gnext-swank")
  ((kbd "s-h") "gprev-swank")
  ((kbd "h") "gprev-swank")
  ((kbd "L") "gnext-with-window")
  ((kbd "H") "gprev-with-window")
  ((kbd "m") "gmove")
  ((kbd "w") "grouplist")
  ((kbd "n") "gnew")
  ((kbd "N") "gnew-float")
  ((kbd "q") "gkill")
  ((kbd "r") "grename")
  ((kbd "R") "ws-rename")
  ((kbd "k") "ws-next")
  ((kbd "j") "ws-prev")
  ((kbd "s") "ws-new")
  ((kbd "W") "ws-select"))

;; (define-key *group-bindings* (kbd "g") "group-interactive-keymap")
(define-key *group-bindings* (kbd "p") "group-update-picture")
(define-key *group-bindings* (kbd "s-p") "group-update-picture")
(define-key *group-bindings* (kbd "G") "group-interactive-keymap")
(define-key *group-bindings* (kbd "g") "display-ws")

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Frame Configuration
(defvar *frame-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "r") "iresize")
    (define-key m (kbd "w") "windowlist")
    (define-key m (kbd "W") "all-windowlist")
    (define-key m (kbd "e") "browser-menu")
    (define-key m (kbd "E") "browser-history")
    (define-key m (kbd "R") "title")
    (define-key m (kbd "b") "balance-frames")
    (define-key m (kbd "c") "toggle-center-frame")
    (define-key m (kbd "m") "mode-line")
    (define-key m (kbd "g") "gaps")
    (define-key m (kbd "f") "fullscreen")
    (define-key m (kbd "l") "set-backlight")
    (define-key m (kbd "L") "reset-backlight")
    (define-key m (kbd "n") "net-scan")
    (define-key m (kbd "N") "vpn-toggle")
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


;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Connect Keymap
;;------------------------------------------------------------------------------------------------------------------------ ;;
;; what should network be under?

;; I can't do the obvious of N for network, but I can do Connect
(defvar *connect-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "n") "net-scan")
    (define-key m (kbd "N") "netctl")
    (define-key m (kbd "v") "list-vpns")
    (define-key m (kbd "k") "kill-vpn")
    (define-key m (kbd "k") '*common-lisp-mode*)
    m ; NOTE: this is important
    ))

(define-key *top-map* (kbd "s-c") '*connect-bindings*)
;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Mode Commands ;;
;; TODO modes should be based on cur-frame

;; Common Lisp Mode
(defvar *common-lisp-mode-repl*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "c") "swank")
    (stumpwm:define-key m (stumpwm:kbd "r") "eval-from-clipboard")
    m))

(defvar *common-lisp-mode*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "s") '*common-lisp-mode-repl*)
    m))

(stumpwm:define-key *common-lisp-mode* (stumpwm:kbd "e") '*common-lisp-mode-repl*)
(stumpwm:undefine-key *top-map* (stumpwm:kbd "s"))
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-m") '*common-lisp-mode*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Emacs Commands ;;
(defvar *emacs-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "e") "eshell")
    m))

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
    (stumpwm:define-key m (stumpwm:kbd "q") "safe-kill")
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

;;      "-t -L 5900:localhost:5900 "

;; ssh -L4006:127.0.0.1:4005 192.168.0.100

;; TODO change from run-shell-command to usocket and inferior-shell
(stumpwm:defcommand vnc-server () ()
  (let* ((password "")
         (remote "192.168.0.100"))
    (stumpwm:run-shell-command (concat "sshpass -p \"" password
                                       "\" ssh -o StrictHostKeyChecking=no -t -L 5900:localhost:5900 "
                                       remote
                                       " 'x11vnc "
                                       "x11vnc -noxdamage -many -display :"
                                       display
                                       "-auth /home/arch/.Xauthority'"))))


(stumpwm:defcommand link-desktop () ()
  (let* ((password "")
         (remote "192.168.0.103"))
    (stumpwm:run-shell-command (concat "sshpass -p \"" password
                                       "\" ssh -o StrictHostKeyChecking=no -L 5900:localhost:5900 "
                                       remote))))


;; (defmacro create-link (name remote local-port remote-port)
;; `(stumpwm:defcommand ,name () ()
;;          (remote ,remote))
;;     (stumpwm:run-shell-command (concat "sshpass -p \"" password
;;                                        "\" ssh -o StrictHostKeyChecking=no -L 5900:localhost:5900 "
;;                                        remote))))


(stumpwm:defcommand vnc-viewer () ()
  (stumpwm:run-shell-command "vncviewer localhost:0"))

;; ;; "modprobe acpi_call"
;; ;; "/usr/share/acpi_call/examples/turn_off_gpu.sh"
