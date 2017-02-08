;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Keymap ;;
;;------------------------------------------------------------------------------------------------------------------------ ;;

(in-package :stumpwm)
;; Main;;

;; (defvar *main-map*
;;   (let ((m (stumpwm:make-sparse-keymap)))
;;     (stumpwm:define-key m (stumpwm:kbd "a") '*application-bindings*)
;;     (stumpwm:define-key m (stumpwm:kbd "g") '*group-bindings*)
;;     (stumpwm:define-key m (stumpwm:kbd "m") '*common-lisp-mode*)
;;     (stumpwm:define-key m (stumpwm:kbd "e") '*emacs-binding*)
;;     m
;;     ))
;; (stumpwm:define-key stumpwm:*top-map* (stumpwm:set-prefix-key "j") *main-map*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Applications ;;
(defvar *application-bindings*
   (let ((m (stumpwm:make-sparse-keymap)))
     (stumpwm:define-key m (stumpwm:kbd "e") "gnext")
     (stumpwm:define-key m (stumpwm:kbd "e") "exec emacs")
     (stumpwm:define-key m (stumpwm:kbd "u") "exec urxvt")
     (stumpwm:define-key m (stumpwm:kbd "s") "exec slack")
     (stumpwm:define-key m (stumpwm:kbd "F2") "exec conky")
     (stumpwm:define-key m (stumpwm:kbd "XF86AudioPlay") "exec spotify")
     (stumpwm:define-key m (stumpwm:kbd "g") "exec google-chrome-stable")
     m ; NOTE: this is important
     ))
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-a") '*application-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Group Configuration ;;
(defvar *group-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "l") "gnext")
    (stumpwm:define-key m (stumpwm:kbd "h") "gprev")
    (stumpwm:define-key m (stumpwm:kbd "L") "gnext-with-window")
    (stumpwm:define-key m (stumpwm:kbd "H") "gprev-with-window")
    (stumpwm:define-key m (stumpwm:kbd "w") "grouplist")
    (stumpwm:define-key m (stumpwm:kbd "n") "gnew")
    (stumpwm:define-key m (stumpwm:kbd "q") "gkill")
    (stumpwm:define-key m (stumpwm:kbd "r") "grename")
    m ; NOTE: this is important
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-g") '*group-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Frame Configuration
(defvar *frame-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "r") "iresize")
    (stumpwm:define-key m (stumpwm:kbd "f") "windowlist")
    (stumpwm:define-key m (stumpwm:kbd "R") "title")
    (stumpwm:define-key m (stumpwm:kbd "b") "balance-frames")
    (stumpwm:define-key m (stumpwm:kbd "m") "mode-line")
    (stumpwm:define-key m (stumpwm:kbd "l") "rofi-run")
    m ; NOTE: this is important
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-f") '*frame-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Mode Commands ;;
;; TODO modes should be based on cur-frame

;; Common Lisp Mode
(defvar *common-lisp-mode-repl*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "c") "swank")
    m
    ))

(defvar *common-lisp-mode*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "s") '*common-lisp-mode-repl*)
    m
    ))

(stumpwm:define-key *common-lisp-mode* (stumpwm:kbd "e") '*common-lisp-mode-repl*)
(stumpwm:undefine-key *top-map* (stumpwm:kbd "s"))
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-m") '*common-lisp-mode*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Emacs Commands ;;
(defvar *emacs-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "e") "eshell")
    m
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-e") '*emacs-bindings*)

;; ------------------------------------------------------------------------------------------------------------------------ ;;
;; Mode Line Commands
;; (defvar *mode-line-bindings*
;;   (let ((m (stumpwm:make-sparse-keymap)))
;;     (stumpwm:define-key m (stumpwm:kbd "R") "title")
;;     (stumpwm:define-key m (stumpwm:kbd "b") "balance-frames")
;;     m ; NOTE: this is important
;;     ))
;; (stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-") '*mode-line-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Database Commands ;;

(defvar *database-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "p") '*postgres-bindings*)
    m
    ))

(defvar *postgres-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "s") "pg-start")
    (stumpwm:define-key m (stumpwm:kbd "q") "pg-stop")
    m
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-d") '*database-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Quit Bindings ;;

(defvar *quit-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "q") "kill")
    m
    ))

(stumpwm:define-key *top-map* (stumpwm:kbd "s-q") '*quit-bindings*)

;;------------------------------------------------------------------------------------------------------------------------ ;;
;; Rofi
(stumpwm:defcommand rofi-run ()
  (inferior-shell:run "rofi -run" t))


