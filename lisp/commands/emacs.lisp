;;-------~---~----------~----------~----
;; *-Controlling Emacs -* ;;
;;-------~---~----------~----------~----
(in-package :stumpwm)

(defcommand emacs (name) ((:string "Name of server: "))
  (cond ((ps-exists "emacs ")
         (stumpwm:run-commands
          (format nil "exec sh -c \"emacs --eval \"(setq server-name \\\"~a\\\" )\" --daemon\"" name))
