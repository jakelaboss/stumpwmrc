;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Command Definitions
;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Emacs Commands

;; (stumpwm:defcommand eshell () ()
;;                     (stumpwm::send-meta-key (stumpwm:current-screen) (stumpwm:meta SPC))
;;                     (stumpwm::send-meta-key (stumpwm:current-screen) (stumpwm:window-send-string "")))


;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Swank Server
(load "/home/arch/.emacs.d/elpa/slime-20161109.640/swank-loader.lisp")
(swank-loader:init)

;; For the not so lazy
(defcommand swank () ()
  (swank:create-server :port 4005
                       :style swank:*communication-style*
                       :dont-close t)
  (echo-string (current-screen)
               "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))

;;------------------------------------------------------------------------------------------------------------------------ ;;

;; Rewrite some commands

;;------------------------------------------------------------------------------------------------------------------------ ;;

(defun shell-command (command) "Run a shell command and display output to screen.
    This must be used in a functional side-effects-free style! If a program does not
    exit of its own accord, Stumpwm might hang!"
       (check-type command string)
       (echo-string (current-screen) (run-shell-command command t)))

(define-stumpwm-command "shell-command" ((command :string "sh: " :string))
  (check-type command string)
  (shell-command command))

(defun cat (&rest strings) "Concatenates strings, like the Unix command 'cat'.
    A shortcut for (concatenate 'string foo bar)."
       (apply 'concatenate 'string strings))
