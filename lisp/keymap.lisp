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
   (let ((m (make-sparse-keymap)))
     ;; (define-key m (kbd "e") "gnext")
     (define-key m (kbd "e") "exec sh -c 'emacsclient -e \"(new-frame)\"'")
     ;; (define-key m (kbd "E") "exec sh -c \"emacs --eval \"(setq server-name \\\"work\\\")\" --daemon\"")
     (define-key m (kbd "E") "exec sh -c 'emacs'")
     ;; (define-key m (kbd "E") "exec sh -c 'emacs -nw --daemon'")
     (define-key m (kbd "a") "exec arandr")
     (define-key m (kbd "v") "exec pavucontrol-qt")
     (define-key m (kbd "u") "exec urxvt")
     (define-key m (kbd "s") "exec rofi -show ssh")
     (define-key m (kbd "S") "exec slack")
     (define-key m (kbd "F2") "exec conky")
     (define-key m (kbd "k") "exec keypass")
     (define-key m (kbd "p") "exec plover")
     (define-key m (kbd "r") "exec rofi -show drun")
     (define-key m (kbd "f") "exec firefox-developer-edition")
     (define-key m (kbd "F") "exec chromium")
     (define-key m (kbd "XF86AudioPlay") "exec spotify")
     (define-key m (kbd "g") "exec google-chrome-beta")
     m ; NOTE: this is important
     ))
(define-key *top-map* (kbd "s-a") '*application-bindings*)

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
    m ; NOTE: this is important
    ))

(define-key *top-map* (kbd "s-f") '*frame-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Appearance Configuration

(defvar *wallpaper-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "o") "set-to-orange")
    (define-key m (kbd "g") "set-to-green")
    (define-key m (kbd "G") "set-to-grass")
    (define-key m (kbd "m") "set-to-mountains")
    (define-key m (kbd "p") "set-to-purple-mountains")
    (define-key m (kbd "w") "set-to-work")
    (define-key m (kbd "l") "set-backlight")
    m ; NOTE: this is important
    ))

(define-key *top-map* (kbd "s-w") '*wallpaper-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Mode Commands ;;
;; TODO modes should be based on cur-frame

;; Common Lisp Mode
(defvar *common-lisp-mode-repl*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "c") "swank")
    m
    ))

(defvar *common-lisp-mode*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") '*common-lisp-mode-repl*)
    m
    ))

(define-key *common-lisp-mode* (kbd "e")
  '*common-lisp-mode-repl*)

(undefine-key *top-map* (kbd "s"))

(define-key *top-map* (kbd "s-m") '*common-lisp-mode*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Emacs Commands ;;
(defvar *emacs-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") "eshell")
    m))

(define-key *top-map* (kbd "s-e") '*emacs-bindings*)

;; ------------------------------------------------------------------------------------------------------------------------ ;;
;; Audio Config ;;

(define-interactive-keymap (audio-control tile-group) ()
  ((kbd "-") "dec-volume")
  ((kbd "+") "inc-volume")
  ((kbd "r") "reset-audio"))

(define-key *top-map* (kbd "s-o") "audio-control")

;; ------------------------------------------------------------------------------------------------------------------------ ;;
;; Mode Line Commands
;; (defvar *mode-line-bindings*
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m (kbd "R") "title")
;;     (define-key m (kbd "b") "balance-frames")
;;     m ; NOTE: this is important
;;     ))
;; (define-key *top-map* (kbd "s-") '*mode-line-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Database Commands ;;

(defvar *database-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p") '*postgres-bindings*)
    m
    ))

(defvar *postgres-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") "pg-start")
    (define-key m (kbd "q") "pg-stop")
    m
    ))

;; (define-key *top-map* (kbd "s-d") '*database-bindings*)
;; (define-key *top-map* (kbd "s-d") '*database-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Quit Bindings ;;

(defvar *quit-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") "kill")
    m
    ))

(define-key *top-map* (kbd "s-q") '*quit-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Toggle Bindings

(defvar *toggle-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") "toggle-golden-ratio-command")
    (define-key m (kbd "p") "plover-toggle")
    (define-key m (kbd "r") "resize-popup")
    (define-key m (kbd "G") "toggle-golden-ratio-toplevel")
    (define-key m (kbd "h") "move-with-ratio left")
    (define-key m (kbd "j") "move-with-ratio down")
    (define-key m (kbd "k") "move-with-ratio up")
    (define-key m (kbd "l") "move-with-ratio right")
    m
    ))

(define-key *top-map* (kbd "s-t") '*toggle-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Golden Ratio Bindings
(defcommand toggle-golden-ratio-toplevel () ()
  (if (null *golden-ratio-toplevel*)
      (progn
        (define-key *top-map* (kbd "s-h") "move-with-ratio left")
        (define-key *top-map* (kbd "s-j") "move-with-ratio down")
        (define-key *top-map* (kbd "s-k") "move-with-ratio up")
        (define-key *top-map* (kbd "s-l") "move-with-ratio right")
        (setf *golden-ratio-toplevel* t))
      (progn
        (define-key *top-map* (kbd "s-j") "move-focus down")
        (define-key *top-map* (kbd "s-h") "move-focus left")
        (define-key *top-map* (kbd "s-k") "move-focus up")
        (define-key *top-map* (kbd "s-l") "move-focus right")
        (setf *golden-ratio-toplevel* nil))))

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Rofi Bindings
(defvar *rofi-bindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "r") "exec rofi -show run")
    (define-key m (kbd "w") "exec rofi -show window")
    (define-key m (kbd "s") "exec rofi -show ssh")
    m
    ))

(define-key *top-map* (kbd "s-u") '*rofi-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Setup Remote Swank Connection

;; (defvar *remote-bindings*
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m (kbd "s-c") "")
;;     (define-key m (kbd "j") "move-focus down")
;;     (define-key m (kbd "h") "move-focus left")
;;     (define-key m (kbd "k") "move-focus up")
;;     (define-key m (kbd "l") "move-focus right")
;;     (define-key m (kbd "J") "move-window down")
;;     (define-key m (kbd "H") "move-window left")
;;     (define-key m (kbd "K") "move-window up")
;;     (define-key m (kbd "L") "move-window right")
;;     ;; move window
;;     (define-key m (kbd "J") "move-window down")
;;     (define-key m (kbd "H") "move-window left")
;;     (define-key m (kbd "K") "move-window up")
;;     (define-key m (kbd "L") "move-window right")
;;     ;; Splits WIndows and Frames
;;     (define-key m (kbd "v") "hsplit")
;;     (define-key m (kbd "s") "vsplit")
;;     (define-key m (kbd "r") "remove")
;;     (define-key m (kbd "q") "kill")
;;     (define-key m (kbd "-")"fclear")
;;     (define-key m (kbd "n") "pull-hidden-next")
;;     (define-key m (kbd "p") "pull-hidden-previous")
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
