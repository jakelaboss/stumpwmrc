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
     (stumpwm:define-key m (stumpwm:kbd "e") "gnext")
     (stumpwm:define-key m (stumpwm:kbd "e") "exec emacs")
     (stumpwm:define-key m (stumpwm:kbd "u") "exec urxvt")
     (stumpwm:define-key m (stumpwm:kbd "s") "exec slack")
     (stumpwm:define-key m (stumpwm:kbd "F2") "exec conky")
     (stumpwm:define-key m (stumpwm:kbd "XF86AudioPlay") "exec spotify")
     (stumpwm:define-key m (stumpwm:kbd "g") "exec qutebrowser")
     m ; NOTE: this is important
     ))
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-a") '*application-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Group Configuration ;;
(defvar *group-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "l") "gnext")
    (stumpwm:define-key m (stumpwm:kbd "h") "gprev")
    (stumpwm:define-key m (stumpwm:kbd "L") "gnext-with-window")
    (stumpwm:define-key m (stumpwm:kbd "H") "gprev-with-window")
    (stumpwm:define-key m (stumpwm:kbd "w") "grouplist")
    (stumpwm:define-key m (stumpwm:kbd "n") "gnew")
    (stumpwm:define-key m (stumpwm:kbd "q") "gkill")
    (stumpwm:define-key m (stumpwm:kbd "r") "grename")
    m ; NOTE: this is important
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-g") '*group-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Frame Configuration
(defvar *frame-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "r") "iresize")
    (stumpwm:define-key m (stumpwm:kbd "f") "windowlist")
    (stumpwm:define-key m (stumpwm:kbd "R") "title")
    (stumpwm:define-key m (stumpwm:kbd "b") "balance-frames")
    (stumpwm:define-key m (stumpwm:kbd "m") "mode-line")
    (stumpwm:define-key m (stumpwm:kbd "l") "rofi-run")
    m ; NOTE: this is important
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-f") '*frame-bindings*)

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

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-d") '*database-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Quit Bindings ;;

(defvar *quit-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "q") "kill")
    m
    ))

(stumpwm:define-key *top-map* (stumpwm:kbd "s-q") '*quit-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Rofi
(stumpwm:defcommand rofi-run ()
  (inferior-shell:run "rofi -run" t))


;; Setup Remote Swank Connection

(defvar *remote-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "s-c") "")
    (stumpwm:define-key m (stumpwm:kbd "j") "move-focus down")
    (stumpwm:define-key m (stumpwm:kbd "h") "move-focus left")
    (stumpwm:define-key m (stumpwm:kbd "k") "move-focus up")
    (stumpwm:define-key m (stumpwm:kbd "l") "move-focus right")
    (stumpwm:define-key m (stumpwm:kbd "J") "move-window down")
    (stumpwm:define-key m (stumpwm:kbd "H") "move-window left")
    (stumpwm:define-key m (stumpwm:kbd "K") "move-window up")
    (stumpwm:define-key m (stumpwm:kbd "L") "move-window right")
    ;; move window
    (stumpwm:define-key m (stumpwm:kbd "J") "move-window down")
    (stumpwm:define-key m (stumpwm:kbd "H") "move-window left")
    (stumpwm:define-key m (stumpwm:kbd "K") "move-window up")
    (stumpwm:define-key m (stumpwm:kbd "L") "move-window right")
    ;; Splits WIndows and Frames
    (stumpwm:define-key m (stumpwm:kbd "v") "hsplit")
    (stumpwm:define-key m (stumpwm:kbd "s") "vsplit")
    (stumpwm:define-key m (stumpwm:kbd "r") "remove")
    (stumpwm:define-key m (stumpwm:kbd "q") "kill")
    (stumpwm:define-key m (stumpwm:kbd "-")"fclear")
    (stumpwm:define-key m (stumpwm:kbd "n") "pull-hidden-next")
    (stumpwm:define-key m (stumpwm:kbd "p") "pull-hidden-previous")
    m
    ))


;; this is a keymap that sends all kestrokes to a a port
;; Can either be the port 4005 for local stumpwm, 4006 for remote stumpwm
;; or 5900 for vnc server



(swank/gray:stream-listen


(swank-api:with-connection (swank-api:*emacs-connection*) (swank:)

(swank-api:send-to-remote-channel 4006 "print 'hello 'world")
(stumpwm:define-key *top-map* (stumpwmw) (window-send-string (format nil)"~a"))

(swank-repl:listener-eval :port 4006 :style swank:*communication-style*
                     :dont-close t)

(let* ((server (usocket:socket-connect "127.0.0.1" 4006))
       (object (stream ))
  (usocket:socket-stream )}))

;; process to create a ssh link between local 4006 port and port on remote 4005 port


;; This will create a vnc-server on a remote server
(defmacro define-ssh-command (name ssh-arguments command)
  "Creates a ssh command to be run on"
  (let* ((nm name)
         (cmd command)
         (ssh ssh-arguments))
  `(stumpwm:defcommand ,nm () ()
    (let* ((password "let'sgoexploring")
           (remote "192.168.0.100"))
      (if (null ,port)
      (stumpwm:run-shell-command (concat "sshpass -p \"" password
                                         "\" ssh -o StrictHostKeyChecking=no "
                                         ,ssh remote " '" ,cmd "'"))))

     "-t -L 5900:localhost:5900 "
;; ssh -L4006:127.0.0.1:4005 192.168.0.100


(stumpwm:defcommand vnc-server () ()
  (let* ((password "let'sgoexploring")
         (remote "192.168.0.100"))
    (stumpwm:run-shell-command (concat "sshpass -p \"" password
                                       "\" ssh -o StrictHostKeyChecking=no -t -L 5900:localhost:5900 "
                                       remote
                                       " 'x11vnc -display :0 -auth /home/arch/.Xauthority'"))))
