;;-------~---~----------~----------~----
;; *-Startup Scripts-* ;;
;;-------~---~----------~----------~----

;; Shell Commands ::

;; Head Configuration
(defvar *main-head*"xrandr --output HDMI-0 --mode 1920x1080  --pos 0x1620 --rate 60.00 --output  DP-1 --mode 3840x2160 --pos 1920x1080 --output DVI-I-1 --mode 1920x1080 --rate 144.00  --pos 5760x1620  --output DVI-D-0 --mode 1920x1080 --pos 2880x0")
(defun head-config ()
  (run-shell-command *main-head*))

(head-config)

;; Keybindings ::

;; Sets Caps-lock to ESC key
(run-shell-command "xmodmap -e 'clear Lock'")
(run-shell-command "xmodmap -e 'keycode 0x42=Escape'")
(run-shell-command "xmodmap ~/.Xmodmap")

;; Set 9 and 0 to [ and ]  for lisp editing
;;(defvar *set-easy-paran*
;;  (do (
;;      (run-shell-command "xmodmap -e 'clear ['")
;;      (run-shell-command "xmodmap -e 'clear ]'")
;;      (run-shell-command "xmodmap -e 'keycode 0x42=Escape'"))))

;;(defvar quicklisp-path "~/quicklisp")
