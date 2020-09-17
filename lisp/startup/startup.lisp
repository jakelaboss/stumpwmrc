;;------------------------------------------------------------------------------------------------------------------------ ;;
;; *-Startup Scripts-* ;;
;;------------------------------------------------------------------------------------------------------------------------ ;;
(in-package :stumpwm)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; ;; ;;   ;; Key Mappings
(run-shell-command "xmodmap -e 'clear Lock'") ;; Sets Caps-lock to ESC key
(run-shell-command "xmodmap -e 'keycode 0x42=Escape'")
(stumpwm:run-shell-command "xmodmap ~/.Xmodmap");; Set Brackets to Parans
(stumpwm:run-shell-command "libinput-gestures-setup start")

(defcommand home-dual () ()
  (run-shell-command "./~/.screenlayout/home-dual.sh"))
;; ;;   )

;; (stumpwm:run-shell-command "set-font -m 8859-2")
;; (sometimes) the external monitors need to be reset

;; (defun displaylink-monitor-reset ()
  ;; (progn
;; (stumpwm:run-shell-command "xrandr --output DVI-I-1-1 --off")
;; (Stumpwm:run-shell-command "xrandr --output DVI-I-1-1 --auto")
;; (stumpwm:run-shell-command "xrandr --output DVI-I-2-2 --off")
;; (stumpwm:run-shell-command "xrandr --output DVI-I-2-2 --auto")
;; (stumpwm:run-shell-command "xrandr --output DVI-I-3-3 --auto")
;; (stumpwm:run-shell-command "xrandr --output DVI-I-3-3 --off")

;; (displaylink-monitor-reset)

;; "xrandr --newmode "1920x1080_60.01"  172.83  1920 2040 2248 2576  1080 1081 1084 1118  -HSync +Vsync"


;; ;; 4k config

(defvar screen-center "--mode 3840x2160 --pos 1920x0 ")
;; (defvar screen-center "--auto")
;; ;; these are matched with the 4k screen to be equadistant
(defvar screen-left "--mode 1920x1080  --pos 0x540 --rate 60.01 ")
(defvar screen-right "--mode 1920x1080 --pos 5760x540 --rate  60.01  ")

;; (defvar screen-left "--mode 1920x1080  --pos 0x1620 --rate 60.01 ")
;; (defvar screen-right "--mode 1920x1080 --pos 5760x1620 --rate  60.01  ")

;; ;; for the 6 screen setup
;; (defvar screen-left "--mode 1920x1080  --pos 0x1080 --rate 60.01 ")
;; (defvar screen-right "--mode 1920x1080 --pos 5760x1080 --rate  60.01  ")
;; (defvar screen-up "--mode 1920x1080 --pos 2880x0 --rate 60.00")
;; (defvar screen-up-virtual "--mode 7840x1080_60.00 --pos 0x0 ")
;; (defvar disable "--off")

;; Xrandr Outputs
(defvar eDP "--output eDP-1 ")
(defvar DP-1 "--output DP-1 ")
(defvar DVI-1 "--output DVI-I-1-1 ")
(defvar DVI-2 "--output DVI-I-2-2 ")
(defvar DVI-3 "--output DVI-I-3-3 ")
(defvar DP-2 "--output DP-2 ")
(defvar HDMI "--output HDMI-1 ")
(defvar Virtual "--output VIRTUAL1 ")

(defvar *main-head*
  ;; (format nil "xrandr ~a ~a ~a ~a ~a ~a"; ~a ~a"
  (print
   (concatenate 'string "xrandr "
                ;; eDP screen-center
                DVI-1 screen-right
                ;; DVI-2 disable
                ;; DVI-2 screen-left
                ;; DP-2 screen-right
                ;; Virtual screen-up
                )))

;; (defun head-config ()
;;   (stumpwm:run-shell-command *main-head*))

;; (print *main-head*)

;; (head-config)

;; (stumpwm:run-shell-command "xrandr --output eDP1 --mode 3840x2160 --pos 0x0")

;; ;;------------------------------------------------------------------------------------------------------------------------ ;;

;; TESTING
;; would love to get this working

;; (defun head-config ()
;;   (let* ((xrandr-config
;;            '((stumpwm:run-shell-command "xrandr --setprovideroutputsource 1 0")
;;              (stumpwm:run-shell-command "xrandr --setprovideroutputsource 2 0")
;;              ;; creates a virtual device to connect an X11 display to
;;              (stumpwm:run-shell-command "xrandr --addmode DVI-I-2-2 1920x1080")))

;;          (key-mappings
;;            '((run-shell-command "xmodmap -e 'clear Lock'") ; Sets Caps-lock to ESC key
;;              (run-shell-command "xmodmap -e 'keycode 0x42=Escape'")
;;              (stumpwm:run-shell-command "xmodmap ~/.Xmodmap")))

;;          (displaylink-monitor-reset (lambda ()
;;            '((stumpwm:run-shell-command "xrandr --output DVI-I-1-1 --off")
;;              (Stumpwm:run-shell-command "xrandr --output DVI-I-1-1 --auto")
;;              (stumpwm:run-shell-command "xrandr --output DVI-I-2-2 --off")
;;              (stumpwm:run-shell-command "xrandr --output DVI-I-2-2 --auto"))))

;;     `(mapcar (lambda (x) (mapcar #'eval x)) (xrandr-config key-mappings)) ; displaylink-monitor-reset))
;;     (let*
;;         ;; Screens
;;         ((screen-center "--mode 3840x2160 --pos 1920x1080 ")
;;          (screen-left "--mode 1920x1080  --pos 0x1620 --rate 60.01 ")
;;          (screen-right "--mode 1920x1080 --pos 5760x1620 --rate  60.01  ")
;;          (screen-up "--mode 1920x1080 --pos 2880x0 --rate 60.00")
;;          ;; not all of these will be used
;;          (eDP "--output eDP1 ")
;;          (DP-1 "--output DP-1 ")
;;          (DVI-1 "--output DVI-I-1-1 ")
;;          (DVI-2 "--output DVI-I-2-2 ")
;;          (DP-2 "--output DP-2 ")
;;          (HDMI "--output HDMI-1 ")
;;          (Virtual "--output VIRTUAL1 ")
;;          ;; xrandr Command
;;          (*main-head*
;;            (format nil "xrandr ~a ~a ~a ~a ~a ~a" ; ~a ~a"
;;                    eDP screen-center
;;                    DVI-1 screen-right
;;                    DVI-2 screen-left
;;                    ;; DP-2 screen-right
;;                    ;; Virtual screen-up
;;                    )))
;;       (stumpwm:run-shell-command *main-head*))))

(defcommand keyboard-reset () ()
  (reset-keyboard))

;; (run-shell-command "libinput-gestures-setup start")
