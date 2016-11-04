;;-------~---~----------~----------~----
;; Keymap ;;
;;-------~---~----------~----------~----

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

;; Group Configuration ;;
(defvar *group-bindings*
  (let ((m (stumpwm:make-sparse-keymap)))
    (stumpwm:define-key m (stumpwm:kbd "l") "gnext")
    (stumpwm:define-key m (stumpwm:kbd "h") "gprev")
    (stumpwm:define-key m (stumpwm:kbd "L") "gnext-with-window")
    (stumpwm:define-key m (stumpwm:kbd "H") "gprev-with-window")
    (stumpwm:define-key m (stumpwm:kbd "w") "grouplist")
    (stumpwm:define-key m (stumpwm:kbd "n") "gnew")
    m ; NOTE: this is important
    ))

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-g") '*group-bindings*)

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

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-m") '*common-lisp-mode*)
(stumpwm:define-key *common-lisp-mode* (stumpwm:kbd "e") '*common-lisp-mode-repl*)
