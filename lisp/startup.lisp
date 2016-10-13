;;-------~---~----------~----------~----
;; *-Startup Scripts-* ;;
;;-------~---~----------~----------~----

;; Shell Commands ::

;; Head Configuration
(defun head-config ()
  (run-shell-command "xrandr --output HDMI-0 --mode 1920x1080  --pos 0x1620 --rate 60.00 --output  DP-1 --mode 3840x2160 --pos 1920x1080 --output DVI-I-1 --mode 1920x1080 --rate 144.00  --pos 5760x1620  --output DVI-D-0 --mode 1920x1080 --pos 2880x0"))

(head-config)

;; Keybindings ::

;; Sets Caps-lock to ESC key
(run-shell-command " xmodmap -e 'clear Lock'")
(run-shell-command "xmodmap -e 'keycode 0x42=Escape'")
(run-shell-command "xmodmap ~/.Xmodmap")
