
;; Not working
(in-package :stumpwm)

(defun emacs-run (command)
  (inferior-shell:run/s
   (format nil "emacsclient -e \"~a\"" command)))


(defun set-emacs-theme (theme)
  (emacs-run (format nil "(load-theme '~a)" (string-downcase theme))))
(defun set-emacs-transparancy (number)
  (emacs-run
   (format nil "(set-frame-parameter (selected-frame) 'alpha '(~a . ~a))" number number))
  (emacs-run
   (format nil "(add-to-list 'default-frame-alist '(alpha . (~a . ~a)))" number number)))
