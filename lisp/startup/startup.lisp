;;------------------------------------------------------------------------------------------------------------------------ ;;
;; *-Startup Scripts-* ;;
;;------------------------------------------------------------------------------------------------------------------------ ;;
(in-package :stumpwm)

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; ;; ;;   ;; Key Mappings

(defun reset-keyboard ()
  (run-shell-command "xmodmap -e 'clear Lock'") ;; Sets Caps-lock to ESC key
  (run-shell-command "xmodmap -e 'keycode 0x42=Escape'")
  (stumpwm:run-shell-command "xmodmap ~/.Xmodmap"));; Set Brackets to Parans

(reset-keyboard)

(defcommand keyboard-reset () ()
  (reset-keyboard))
