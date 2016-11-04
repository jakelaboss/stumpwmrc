;;-------~---~----------~----------~----
;; *-Controlling Emacs -* ;;
;;-------~---~----------~----------~----

(send-meta-key (current-screen) (kbd "M-x"))

(ql:quickload 'stumpwm)
(require 'stumpwm)
(stumpwm:)
