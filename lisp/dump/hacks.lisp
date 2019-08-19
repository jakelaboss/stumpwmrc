(in-package :stumpwm)

(defun input-hack (game window-name key)
  (run-shell-command (format nil "xdotool key --window ~a --repeat 10000 --repeat-delay 10 ~a"
                             (inferior-shell:run/ss (format nil "xdotool search -all --pid ~a --name ~a"
                                                            (inferior-shell:run/ss (format nil "pgrep -u vagabond ~a" game))
                                                            window-name))
                             key)))

(let ((game "plover") (window-name "plover"))
  (run-shell-command (format nil "xdotool type --window ~a \"erfvol\""
                             (inferior-shell:run/ss (format nil "xdotool search -all --pid ~a --name ~a"
                                                            (inferior-shell:run/ss (format nil "pgrep -u vagabond ~a" game))
                                                            window-name)))))

(defcommand cuphead-hack () ()
  (input-hack "Cuphead.exe" "Cuphead" "Tab"))

(defcommand morrowind-hack () ()
  (run-shell-command (format nil "xdotool click 1 --window ~a --repeat 1000 --repeat-delay 100"
                             (inferior-shell:run/ss (format nil "xdotool search -all --pid ~a --name ~a"
                                                            (inferior-shell:run/ss (format nil "pgrep -u vagabond ~a" "openmw"))
                                                            "openmw")))))

(defcommand emacs-hack () ()
  (run-shell-command  "xdotool key --window ~a --repeat 1000 --repeat-delay 1000 0"))

(setf (symbol-function 'project-m)
      (let* ((pid (inferior-shell:run/ss (format nil "pgrep -u vagabond ~a" "ishiiruka")))
             (window (inferior-shell:run/ss (format nil "xdotool search -all --pid ~a --name ~a" pid "Brawl"))))
        (lambda (command)
          (run-shell-command (print (format nil "xdotool ~a"
                                      (apply #'concat (loop for x from 0 to (- (length command) 1)
                                                            collect (format nil "key --window ~a ~a " window (char command x))))))))))


(setf (symbol-function 'project-m)
      (let* ((pid (inferior-shell:run/ss (format nil "pgrep -u vagabond ~a" "ishiiruka")))
             (window (inferior-shell:run/ss (format nil "xdotool search -all --pid ~a --name ~a" pid "Brawl"))))
        (lambda (command)
          (run-shell-command (print (format nil "xdotool ~a"
                                            (format nil "key --clearmodifier --delay 100 --window ~a ~a " window command)))))))

(project-m "y+a+a")


(defcommand pm-command (command) ((:string command))
  (project-m command))
                             ;; (find-if #'(lambda (x) (cl-ppcre:scan "Brawl" (window-name x)))
                             ;;          (group-windows (find-group (current-screen) "media")))))



;; (defvar *project-m-bindings*
;;   (let ((m (stumpwm:make-sparse-keymap)))
;;     (stumpwm:define-key m (stumpwm:kbd "j") "project-m yaa")
;;     (stumpwm:define-key m (stumpwm:kbd "m") "project-m yfb")
;;     m
;;     ))
(project-m "yaa")

(inferior-shell:run/ss (format nil "xdotool search -all --pid ~a --name ~a"
                               1234
                               ;; (inferior-shell:run/ss (format nil "pgrep -u vagabond ~a" ""))
                               "Brawl"))

(window)

(define-interactive-keymap (project-m-mode tile-group) ()
  ((stumpwm:kbd "k") "pm-command y+a+a")
  ((stumpwm:kbd "l") "pm-command y+f+b")
  ((stumpwm:kbd "h") "pm-command y+y+s+s+b+b"))

(define-key *top-map* (kbd "s-e") "project-m-mode")

(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "s-e") '*project-m-bindings*)

(stumpwm:define-key *top-map* (stumpwm:kbd "s-u") '*rofi-bindings*)

(define-key *common-lisp-mode* (kbd "c") "emacs-hack")
