;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Modules For Stumpwm ;;
;;------------------------------------------------------------------------------------------------------------------------ ;;

(dolist (m '(
             ;; modeline
             "cpu"
             "mem"
             "hostname"
             "net"
             ;; util
             "app-menu"))
  (stumpwm:load-module m))


(stumpwm:load-module "ttf-fonts")
