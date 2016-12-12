;;-------~---~----------~----------~----
;;; Toggle Commands ;;
;;---~----~-~-------------~---------~---


;; Swank Toggle
;; (defcommand swank () ()
;;   "Turn on the swank server the first time.
;; Load a file that re-defines swank and then calls it."
;;   ;; Be careful with the quotes!
;;   (run-shell-command  "stumpish 'eval (load \"/home/arch/common-lisp/stump/stump.lisp/\")'")
;;   (echo-string
;;    (current-screen)
;;    "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))
;; ;;

;; (ql:quickload :swank)

(load "/home/arch/.emacs.d/elpa/slime-20161109.640/swank-loader.lisp")
(swank-loader:init)

;; For the not so lazy
(defcommand swank () ()
  (swank:create-server :port 4005
                       :style swank:*communication-style*
                       :dont-close t)
  (echo-string (current-screen) 
               "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))
;; (swank)

;; sudo pptpsetup --create municity --server 24.39.106.238 --username scavpn02 --password 71Hook45Red --encrypt 
;; sudo pptpsetup --delete municity 

;; connect to postgresql database database ;;



;; (defcommand postgres () ()
;;   "Turn on the swank server the first time.
;; Load a file that re-defines swank and then calls it."
;;   ;; Be careful with the quotes!
;;   (run-shell-command  "stumpish 'eval (load \"/home/arch/common-lisp/stump/core.lisp/\")'")
;;   (echo-string
;;    (current-screen)
;;    "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))
;; ;;
