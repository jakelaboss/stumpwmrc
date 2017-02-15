;;------------------------------------------------------------------------------------------------------------------------ ;;
;; *-Startup Scripts-* ;;
;;------------------------------------------------------------------------------------------------------------------------ ;;

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Head Configuration

;; This is run so that extrearnal monitors can be recognized
(stumpwm:run-shell-command "xrandr --setprovideroutputsource 1 0")
;; 4k config
(defvar screen-center "--mode 3840x2160 --pos 1920x1080 ")
(defvar screen-left "--mode 1920x1080  --pos 0x1620 --rate 60.01 ")
;; (defvar screen-right "--mode 1920x1080 --rate 144.00  --pos 5760x1620 ")
;; (defvar screen-up "--mode 1920x1080 --pos 2880x0 ")

(defvar eDP "--output eDP1 ")
(defvar DP-1 "--output DP-1 ")
(defvar DVI "--output DVI-I-1-1")
(defvar DP-2 "--output DP-2 ")
(defvar HDMI "--output HDMI-1 ")

(defvar *main-head*
  (format nil "xrandr ~a ~a ~a ~a" ; ~a ~a";~a ~a"
          eDP screen-center
          DVI screen-left
          ;; DP-2 screen-right
          ;; DVI-0 screen-up
          ))

(defun head-config ()
  (stumpwm:run-shell-command *main-head*))

(print *main-head*)

(head-config)


;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Keybindings ::

;; Sets Caps-lock to ESC key
;; Set Brackets to Parans
;; (run-shell-command "xmodmap -e 'clear Lock'")
;; (run-shell-command "xmodmap -e 'keycode 0x42=Escape'")
;; (stumpwm:run-shell-command "xmodmap ~/.Xmodmap")


;;------------------------------------------------------------------------------------------------------------------------ ;;
;; VPN ;;

;; (defun vpn (conf)
;;   (concatenate 'string "sudo openvpn /etc/openvpn/" (concatenate 'string conf ".conf")))

;; (vpn "Brazil")


;;------------------------------------------------------------------------------------------------------------------------ ;;
