;;------------------------------------------------------------------------------------------------------------------------ ;;
;; *-Startup Scripts-* ;;
;;------------------------------------------------------------------------------------------------------------------------ ;;

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Head Configuration

;; 4k config
(defvar screen-center "--mode 3840x2160 --pos 1920x1080 ")
(defvar screen-left "--mode 1920x1080  --pos 0x1620 --rate 60.00 ")
(defvar screen-right "--mode 1920x1080 --rate 144.00  --pos 5760x1620 ")
(defvar screen-up "--mode 1920x1080 --pos 2880x0 ")

;; ;; 2k config
;; (defvar screen-center "--mode 2560x1440 --pos 1920x1080 ")
;; (defvar screen-left "--mode 1920x1080  --pos 0x1260 --rate 60.00 ") ;; (defvar screen-right "--mode 1920x1080 --rate 144.00  --pos 4480x1260 ") ;; (defvar screen-up "--mode 1920x1080 --pos 2560x0 ")

(defvar DP "--output DP-1 ")
(defvar HDMI "--output HDMI-0 ")
(defvar DVI-0 "--output DVI-D-0 ")
(defvar DVI-1 "--output DVI-I-1 ")


(defvar *main-head*
  (format nil "xrandr ~a ~a ~a ~a ~a ~a";~a ~a" 
          DP screen-center
          HDMI screen-left
          DVI-1 screen-right
          ;; DVI-0 screen-up
          ))

(defun head-config ()
  (stumpwm:run-shell-command *main-head*))

(head-config)


;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Keybindings ::

;; Sets Caps-lock to ESC key
;; Set Brackets to Parans
(run-shell-command "xmodmap -e 'clear Lock'")
(run-shell-command "xmodmap -e 'keycode 0x42=Escape'")
(stumpwm:run-shell-command "xmodmap ~/.Xmodmap")


;;------------------------------------------------------------------------------------------------------------------------ ;;
;; VPN ;;

;; (defun vpn (conf)
;;   (concatenate 'string "sudo openvpn /etc/openvpn/" (concatenate 'string conf ".conf")))

;; (vpn "Brazil")


;;------------------------------------------------------------------------------------------------------------------------ ;;
